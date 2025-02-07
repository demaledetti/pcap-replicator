module PcapReplicator.Log (
    stdoutTextTracer,
    stdoutIOTextTracer,
    TracerIOIOT,
    tshow,
) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Tracer (Tracer (..), emit)
import Data.Text qualified as T
import Say (say)

stdoutTextTracer :: (MonadIO m) => Tracer m T.Text
stdoutTextTracer = Tracer $ emit say

stdoutIOTextTracer :: (MonadIO m) => Tracer m (IO T.Text)
stdoutIOTextTracer = Tracer $ emit (liftIO >=> say)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

type TracerIOIOT = Tracer IO (IO T.Text)
