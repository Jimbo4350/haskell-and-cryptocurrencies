module Tree where

import           Control.Monad       (void)
import qualified Control.Monad.State as S
import           Optics

data Tree a = Tip | Node (Tree a) a (Tree a)
    deriving Show

_Tip :: Prism' (Tree a) ()
_Tip = prism' pr (const Tip)
  where
    pr Tip = Just ()
    pr _   = Nothing

_Node :: Prism (Tree a) (Tree b) (Tree a, a, Tree a) (Tree b, b, Tree b)
_Node = prism pr rv
  where
    pr Tip          = Left Tip
    pr (Node l a r) = Right (l, a, r)

    rv (l, b, r) = Node l b r

inorder :: Traversal (Tree a) (Tree b) a b
inorder _ Tip          = pure Tip
inorder f (Node l a r) = Node <$> inorder f l <*> f a <*> inorder f r

preorder :: Traversal (Tree a) (Tree b) a b
preorder _ Tip          = pure Tip
preorder f (Node l a r) = (\a' l' r' -> Node l' a' r') <$> f a <*> preorder f l <*> preorder f r

postorder :: Traversal (Tree a) (Tree b) a b
postorder _ Tip          = pure Tip
postorder f (Node l a r) = (\l' r' a' -> Node l' a' r') <$> postorder f l <*> postorder f r <*> f a

tree :: Tree Char                  --        c
tree = Node                        --       / \
            (Node                  --      /   \
                (Node Tip 'a' Tip) --     b     d
                'b'                --    / \   / \
                Tip)               --   /
            'c'                    --  a
            (Node Tip 'd' Tip)     -- / \

printNodes :: Show a => Traversal' (Tree a) a -> Tree a -> IO ()
printNodes t x = void $ t (\a -> print a >> return a) x

labelNodes :: Traversal (Tree a) (Tree (a, Int)) a (a, Int) -> Tree a -> Tree (a, Int)
labelNodes t x = S.evalState (t m x) 1
  where
    m a = do
        i <- S.get
        S.put (i + 1)
        return (a, i)
