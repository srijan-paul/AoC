module Queue
  ( empty,
    isEmpty,
    push,
    pop,
    size,
    fromList,
    toList,
    singleton,
    enqueueAll,
    enqueue,
    Queue,
  )
where

data Queue a = Queue
  { queueIn :: [a],
    queueOut :: [a]
  }
  deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push a queue@(Queue qIn qOut) = queue {queueIn = a : qIn}

pop :: Queue a -> (a, Queue a)
pop queue@(Queue [] []) = error "pop: Queue in empty"
pop queue@(Queue qIn []) = pop (Queue [] (reverse qIn))
pop queue@(Queue _ (a : as)) = (a, queue {queueOut = as})

enqueue :: a -> Queue a -> Queue a
enqueue x queue@(Queue qIn _) = queue { queueIn = x:qIn }

enqueueAll :: (Foldable f) => f a -> Queue a -> Queue a
enqueueAll xs queue = foldl (flip enqueue) queue xs

peek :: Queue a -> a
peek = fst . pop

size :: Queue a -> Int
size (Queue qIn qOut) = length qIn + length qOut

fromList :: [a] -> Queue a
fromList xs = Queue [] (reverse xs)

toList :: Queue a -> [a]
toList (Queue qIn qOut) = reverse qOut ++ qIn

singleton :: a -> Queue a
singleton x = Queue [] [x]

isEmpty :: Queue a -> Bool
isEmpty (Queue qIn qOut) = length qIn + length qOut == 0
