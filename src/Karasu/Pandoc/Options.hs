-- | Pandoc options
module Karasu.Pandoc.Options (defKarasuReaderOptions, defKarasuWriterOptions) where

import Text.Pandoc

-- | Default reader options for Karasu
defKarasuReaderOptions :: ReaderOptions
defKarasuReaderOptions= def
    { readerExtensions = enableExtension Ext_smart pandocExtensions }


-- | Default writer options for Karasu
defKarasuWriterOptions :: WriterOptions
defKarasuWriterOptions = def
    { writerExtensions = enableExtension Ext_smart pandocExtensions }
