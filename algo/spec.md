Specification
=============

This file consitutes a verifable specification of what the keep-formation algorithm
does, based on editing action. The editing action and result is described concisely
in the code sections.

Let's start with this block:

```
 resolveLineColPos :: LineColPos -> M BufferPos
 resolveLineColPos (lineNr, colNr) = do
     Buffer{..} <- get

     return $ runST $ do
         pos <- newSTRef 0
         lineNumR <- newSTRef 1

         forM_ (T.lines emacsBuffer) $ \line -> do
             lineNum <- readSTRef lineNumR
             modifySTRef lineNumR (+1)
             if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
                                          return ()
                | lineNum == lineNr -> do modifySTRef pos (+ colNr)
                                          return ()
                | otherwise         -> return ()
```

Here are the edits and along with their results:

```
 resolveLineColPos :: LineColPos -> M BufferPos
 resolveLineColPos (lineNr, colNr) = do
     Buffer{..} <- get

     return $ runST $ do
         pos <- newSTRef 0
         lineNumR <- newSTRef 1

         forM_ (T.lines emacsBuffer) $ \line -> do
             lineNum <- readSTRef lineNumR
             modifySTRef lineNumR (+1)
             if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
                                           return ()
                | lineNum == lineNr -> do modifySTRef pos (+ colNr)
                                           return ()
-               | otherwise         -> return ()
x                    ^^^
+               | othise            -> return ()
```

```
 resolveLineColPos :: LineColPos -> M BufferPos
 resolveLineColPos (lineNr, colNr) = do
     Buffer{..} <- get

     return $ runST $ do
         pos <- newSTRef 0
         lineNumR <- newSTRef 1

         forM_ (T.lines emacsBuffer) $ \line -> do
             lineNum <- readSTRef lineNumR
             modifySTRef lineNumR (+1)
-            if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
-                                         return ()
-               | lineNum == lineNr -> do modifySTRef pos (+ colNr)
i                     "2"
-                                         return ()
-               | otherwise         -> return ()
+            if | lineNum < lineNr   -> do modifySTRef pos (+ (1 + T.length line))
+                                          return ()
+               | line2Num == lineNr -> do modifySTRef pos (+ colNr)
+                                          return ()
+               | otherwise          -> return ()
```

```
 resolveLineColPos :: LineColPos -> M BufferPos
 resolveLineColPos (lineNr, colNr) = do
     Buffer{..} <- get

     return $ runST $ do
         pos <- newSTRef 0
         lineNumR <- newSTRef 1

         forM_ (T.lines emacsBuffer) $ \line -> do
             lineNum <- readSTRef lineNumR
             modifySTRef lineNumR (+1)
-            if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
-                                         return ()
-               | lineNum == lineNr -> do modifySTRef pos (+ colNr)
x                     ^
-                                         return ()
-               | otherwise         -> return ()
+            if | lineNum < lineNr -> do modifySTRef pos (+ (1 + T.length line))
+                                        return ()
+               | lineum == lineNr -> do modifySTRef pos (+ colNr)
+                                        return ()
+               | otherwise        -> return ()
```

```
 resolveLineColPos :: LineColPos -> M BufferPos
 resolveLineColPos (lineNr, colNr) = do
     Buffer{..} <- get

     return $ runST $ do
-        pos <- newSTRef 0
i       " "
-        lineNumR <- newSTRef 1
-
-        forM_ (T.lines emacsBuffer) $ \line -> do
-            lineNum <- readSTRef lineNumR
-            modifySTRef lineNumR (+1)
-            if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
-                                         return ()
-               | lineNum == lineNr -> do modifySTRef pos (+ colNr)
-                                         return ()
-               | otherwise         -> return ()
+         pos <- newSTRef 0
+         lineNumR <- newSTRef 1
+
+         forM_ (T.lines emacsBuffer) $ \line -> do
+             lineNum <- readSTRef lineNumR
+             modifySTRef lineNumR (+1)
+             if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
+                                          return ()
+                | lineNum == lineNr -> do modifySTRef pos (+ colNr)
+                                          return ()
+                | otherwise         -> return ()
```

```
 resolveLineColPos :: LineColPos -> M BufferPos
 resolveLineColPos (lineNr, colNr) = do
     Buffer{..} <- get

     return $ runST $ do
-        pos <- newSTRef 0
i        " "
-        lineNumR <- newSTRef 1
+         pos <- newSTRef 0
+         lineNumR <- newSTRef 1

         forM_ (T.lines emacsBuffer) $ \line -> do
             lineNum <- readSTRef lineNumR
             modifySTRef lineNumR (+1)
             if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
                                          return ()
                | lineNum == lineNr -> do modifySTRef pos (+ colNr)
                                          return ()
                | otherwise         -> return ()
```

```
 resolveLineColPos :: LineColPos -> M BufferPos
 resolveLineColPos (lineNr, colNr) = do
     Buffer{..} <- get

     return $ runST $ do
-         pos <- newSTRef 0
x        ^
-         lineNumR <- newSTRef 1
+        pos <- newSTRef 0
+        lineNumR <- newSTRef 1

         forM_ (T.lines emacsBuffer) $ \line -> do
             lineNum <- readSTRef lineNumR
             modifySTRef lineNumR (+1)
             if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
                                          return ()
                | lineNum == lineNr -> do modifySTRef pos (+ colNr)
                                          return ()
                | otherwise         -> return ()
```

```
 resolveLineColPos :: LineColPos -> M BufferPos
 resolveLineColPos (lineNr, colNr) = do
     Buffer{..} <- get

     return $ runST $ do
         pos <- newSTRef 0
         lineNumR <- newSTRef 1

         forM_ (T.lines emacsBuffer) $ \line -> do
             lineNum <- readSTRef lineNumR
             modifySTRef lineNumR (+1)
-            if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
i               "x"
-                                         return ()
-               | lineNum == lineNr -> do modifySTRef pos (+ colNr)
-                                         return ()
-               | otherwise         -> return ()
+            if x| lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
+                                          return ()
+                | lineNum == lineNr -> do modifySTRef pos (+ colNr)
+                                          return ()
+                | otherwise         -> return ()

             next
```

```
 insertText :: Text -> M ()
 insertText t = do
     Buffer{..} <- get
     appendLog $ T.concat $ map T.pack ["Added ", show t]
     put $ Buffer {
           emacsBuffer =
-                T.concat [ T.take emacsPoint emacsBuffer
i                         " "
-                         , t
-                         , T.drop emacsPoint emacsBuffer
-                         ]
+                T.concat  [ T.take emacsPoint emacsBuffer
+                          , t
+                          , T.drop emacsPoint emacsBuffer
+                          ]
         , emacsPoint = emacsPoint + T.length t
         , actionLog = actionLog
         }
```

```
 insertText :: Text -> M ()
 insertText t = do
     Buffer{..} <- get
     appendLog $ T.concat $ map T.pack ["Added ", show t]
     put $ Buffer {
           emacsBuffer =
-                T.concat  [ T.take emacsPoint emacsBuffer
x                         ^
-                          , t
-                          , T.drop emacsPoint emacsBuffer
-                          ]
+                T.concat [ T.take emacsPoint emacsBuffer
+                         , t
+                         , T.drop emacsPoint emacsBuffer
+                         ]
         , emacsPoint = emacsPoint + T.length t
         , actionLog = actionLog
         }
```

```
 insertText :: Text -> M ()
 insertText t = do
     Buffer{..} <- get
     appendLog $ T.concat $ map T.pack ["Added ", show t]
     put $ Buffer
-        { emacsBuffer =
-                 T.concat [ T.take emacsPoint emacsBuffer
-                          , t
-                          , T.drop emacsPoint emacsBuffer
-                          ]
-        , emacsPoint = emacsPoint + T.length t
-        , actionLog = actionLog
i                    "   "
+        { emacsBuffer  =
+                  T.concat [ T.take emacsPoint emacsBuffer
+                           , t
+                           , T.drop emacsPoint emacsBuffer
+                           ]
+        , emacsPoint   = emacsPoint + T.length t
+        , actionLog    = actionLog
         }
```

```
 insertText :: Text -> M ()
 insertText t = do
     Buffer{..} <- get
     appendLog $ T.concat $ map T.pack ["Added ", show t]
     put $ Buffer {
           emacsBuffer =
-                T.concat [ T.take emacsPoint emacsBuffer
x             ^
-                         , t
-                         , T.drop emacsPoint emacsBuffer
-                         ]
+               T.concat [ T.take emacsPoint emacsBuffer
+                        , t
+                        , T.drop emacsPoint emacsBuffer
+                        ]
         , emacsPoint = emacsPoint + T.length t
         , actionLog = actionLog
         }
```
