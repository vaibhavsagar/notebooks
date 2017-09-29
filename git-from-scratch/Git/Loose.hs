{-# LANGUAGE OverloadedStrings #-}

module Git.Loose (module Git.Loose) where

import qualified Codec.Compression.Zlib           as Z (compress, decompress)
import           Control.Monad                         (unless)
import           Data.Attoparsec.ByteString            (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Byteable
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                  as B
import           Data.ByteString.Base16                (encode, decode)
import           Data.ByteString.Lazy                  (fromStrict, toStrict)
import           Data.ByteString.UTF8                  (fromString, toString)
import           Data.Digest.Pure.SHA
import           Data.Monoid                           ((<>), mconcat)
import           System.Directory                      (doesPathExist, createDirectoryIfMissing)
import           System.FilePath                       ((</>), takeDirectory)

type Ref = ByteString

data Commit = Commit
    { commitTree      :: Ref
    , commitParents   :: [Ref]
    , commitAuthor    :: ByteString
    , commitCommitter :: ByteString
    , commitMessage   :: ByteString
    } deriving (Eq, Show)

data Tree = Tree { treeEntries :: [TreeEntry] } deriving (Eq, Show)

data TreeEntry = TreeEntry
    { treeEntryPerms :: ByteString
    , treeEntryName  :: ByteString
    , treeEntryRef   :: Ref
    } deriving (Eq, Show)

data Blob = Blob { blobContent :: ByteString } deriving (Eq, Show)

data Tag = Tag
    { tagObject     :: Ref
    , tagType       :: ByteString
    , tagTag        :: ByteString
    , tagTagger     :: ByteString
    , tagAnnotation :: ByteString
    } deriving (Eq, Show)

data GitObject
    = GitCommit Commit
    | GitTree   Tree
    | GitBlob   Blob
    | GitTag    Tag
    deriving (Eq, Show)

instance Byteable Commit where
    toBytes (Commit cTree cParents cAuthor cCommitter cMessage) = mconcat
        [                       "tree "      <> cTree      <> "\n"
        , mconcat (map (\ref -> "parent "    <> ref        <> "\n") cParents)
        ,                       "author "    <> cAuthor    <> "\n"
        ,                       "committer " <> cCommitter <> "\n"
        ,                                                     "\n"
        ,                                       cMessage
        ]

instance Byteable TreeEntry where
    toBytes (TreeEntry perms name ref) = mconcat [perms, " ", name, "\NUL", fst $ decode ref]

instance Byteable Tree where
    toBytes (Tree entries) = mconcat (map toBytes entries)

instance Byteable Blob where
    toBytes (Blob content) = content

instance Byteable Tag where
    toBytes (Tag tObject tType tTag tTagger tAnnotation) = mconcat
        [ "object " <> tObject     <> "\n"
        , "type "   <> tType       <> "\n"
        , "tag "    <> tTag        <> "\n"
        , "tagger " <> tTagger     <> "\n"
        ,                             "\n"
        ,              tAnnotation
        ]

instance Byteable GitObject where
    toBytes obj = case obj of
        GitCommit c -> withHeader "commit" (toBytes c)
        GitTree   t -> withHeader "tree"   (toBytes t)
        GitBlob   b -> withHeader "blob"   (toBytes b)
        GitTag    t -> withHeader "tag"    (toBytes t)

parsed :: Parser a -> ByteString -> a
parsed parser = either error id . A.parseOnly parser

compress, decompress :: ByteString -> ByteString
compress   = toStrict . Z.compress   . fromStrict
decompress = toStrict . Z.decompress . fromStrict

parseHeader :: Parser (ByteString, Int)
parseHeader = do
    objectType <- A.takeTill A.isSpace
    A.space
    len <- A.decimal
    A.char '\NUL'
    return (objectType, len)

parseHexRef :: Parser Ref
parseHexRef = A.take 40

parseCommit :: Parser Commit
parseCommit = do
    cTree      <-          A.string "tree"      *> A.space *> parseHexRef                 <* A.endOfLine
    cParents   <- A.many' (A.string "parent"    *> A.space *> parseHexRef                 <* A.endOfLine)
    cAuthor    <-          A.string "author"    *> A.space *> A.takeTill (A.inClass "\n") <* A.endOfLine
    cCommitter <-          A.string "committer" *> A.space *> A.takeTill (A.inClass "\n") <* A.endOfLine
    A.endOfLine
    cMessage   <- A.takeByteString
    return $ Commit cTree cParents cAuthor cCommitter cMessage

withHeader :: ByteString -> ByteString -> ByteString
withHeader oType content = mconcat [oType, " ", fromString . show $ B.length content, "\NUL", content]

hash :: ByteString -> Ref
hash = fromString . showDigest . sha1 . fromStrict

parseBinRef :: Parser Ref
parseBinRef = encode <$> A.take 20

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = do
    perms <- fromString <$> A.many1' A.digit
    A.space
    name  <- A.takeWhile (/='\NUL')
    A.char '\NUL'
    ref   <- parseBinRef
    return $ TreeEntry perms name ref

parseTree :: Parser Tree
parseTree = Tree <$> A.many' parseTreeEntry

parseBlob :: Parser Blob
parseBlob = Blob <$> A.takeByteString

parseTag :: Parser Tag
parseTag = do
    tObject     <- A.string "object" *> A.space *> parseHexRef                                               <* A.endOfLine
    tType       <- A.string "type"   *> A.space *> A.choice (map A.string ["commit", "tree", "blob", "tag"]) <* A.endOfLine
    tTag        <- A.string "tag"    *> A.space *> A.takeTill (A.inClass "\n")                               <* A.endOfLine
    tTagger     <- A.string "tagger" *> A.space *> A.takeTill (A.inClass "\n")                               <* A.endOfLine
    A.endOfLine
    tAnnotation <- A.takeByteString
    return $ Tag tObject tType tTag tTagger tAnnotation


parseGitObject :: Parser GitObject
parseGitObject = do
    headerLen <- parseHeader
    case fst headerLen of
        "commit" -> GitCommit <$> parseCommit
        "tree"   -> GitTree   <$> parseTree
        "blob"   -> GitBlob   <$> parseBlob
        "tag"    -> GitTag    <$> parseTag
        _        -> error "not a git object"

hashObject :: GitObject -> Ref
hashObject = hash . toBytes

refPath :: FilePath -> Ref -> FilePath
refPath gitDir ref = let
   (dir,file) = splitAt 2 (toString ref)
   in gitDir </> "objects" </> dir </> file

readObject :: FilePath -> Ref -> IO GitObject
readObject gitDir ref = do
    let path =  refPath gitDir ref
    content  <- decompress <$> B.readFile path
    return $ parsed parseGitObject content

writeObject :: FilePath -> GitObject -> IO Ref
writeObject gitDir object = do
    let ref  =  hashObject object
    let path =  refPath gitDir ref
    exists   <- doesPathExist path
    unless exists $ do
        let dir = takeDirectory path
        createDirectoryIfMissing True dir
        B.writeFile path . compress $ toBytes object
    return ref
