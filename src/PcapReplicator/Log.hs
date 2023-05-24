module PcapReplicator.Log (
    stdoutTextTracer
  , stdoutIOTextTracer
  , tshow
) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Tracer (emit, Tracer(..))
import qualified Data.Text as T
import Say (say)

stdoutTextTracer :: MonadIO m => Tracer m T.Text
stdoutTextTracer = Tracer $ emit say

stdoutIOTextTracer :: MonadIO m => Tracer m (IO T.Text)
stdoutIOTextTracer = Tracer $ emit (liftIO >=> say)

tshow :: Show a => a -> T.Text
tshow = T.pack . show
