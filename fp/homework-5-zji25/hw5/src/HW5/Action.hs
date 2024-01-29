{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase  #-}

module HW5.Action
  ( HiPermission(..)
  , PermissionException(..)
  , HIO(..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.ByteString as B (readFile, writeFile)
import qualified Data.Sequence as S (fromList)
import Data.Set (Set, member)
import qualified Data.Text as T (pack)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.IO as TIO (putStrLn)
import Data.Time.Clock (getCurrentTime)
import HW5.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory,
                         setCurrentDirectory)
import System.Random (randomRIO)

data HiPermission =
  AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (Set HiPermission) IO)


instance HiMonad HIO where
  runAction = \case
    HiActionRead path -> check AllowRead >> liftIO (do
      isFile <- doesFileExist path
      if isFile then do
        bs <- B.readFile path
        return $ either (const $ HiValueBytes bs) HiValueString $ decodeUtf8' bs
      else HiValueList . S.fromList . (map $ HiValueString . T.pack) <$> listDirectory path)
    HiActionWrite path bs -> check AllowWrite >> liftIO (HiValueNull <$ B.writeFile path bs)
    HiActionMkDir path -> check AllowWrite >> liftIO (HiValueNull <$ createDirectory path)
    HiActionChDir path -> check AllowRead >> liftIO (HiValueNull <$ setCurrentDirectory path)
    HiActionCwd -> check AllowRead >> liftIO (HiValueString . T.pack <$> getCurrentDirectory)
    HiActionNow -> check AllowTime >> liftIO (HiValueTime <$> getCurrentTime)
    HiActionRand lo hi -> liftIO (HiValueNumber . toRational <$> randomRIO (lo, hi))
    HiActionEcho txt -> check AllowWrite >> liftIO (HiValueNull <$ TIO.putStrLn txt)
    where
      check perm = HIO $ \set -> unless (member perm set) $ throwIO $ PermissionRequired perm
