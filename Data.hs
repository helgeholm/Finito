module Data where

import List (intersperse)

data Name = Name String
data Section = Section Name [Assign]
data Cond = SimpleCond Atom
          | RestrCond Atom OrRestrictor
data OrRestrictor = SimpleOrRestrictor AndRestrictor
                  | ProperOrRestrictor AndRestrictor OrRestrictor
data AndRestrictor = SimpleAndRestrictor NotRestrictor
                   | ProperAndRestrictor NotRestrictor AndRestrictor
data NotRestrictor = NegateRestrictor InnerRestrictor
                   | PosateRestrictor InnerRestrictor
data InnerRestrictor = NameRestrictor Name
                     | ParenRestrictor OrRestrictor
data Assign = NotReallyAssign Cond
            | ProperAssign Cond [Cond]
data Atom = NameAtom Name
          | StringAtom String

instance Show Assign where
  show (NotReallyAssign c) = show c
  show (ProperAssign c cl) = (show c) ++ " = " ++ clist where
    clist = concat $ intersperse ", " $ map show cl

instance Show Section where
  show (Section n as) = "{" ++ (show n) ++ ": " ++ assigns ++ "}" where
    assigns = concat $ intersperse "; " $ map show as

instance Show Name where
  show (Name n) = n

instance Show Atom where
  show (NameAtom   n) = show n
  show (StringAtom s) = "\"" ++ s ++ "\""

instance Show InnerRestrictor where
  show (NameRestrictor a)  = show a
  show (ParenRestrictor a) = "(" ++ (show a) ++ ")"

instance Show NotRestrictor where
  show (NegateRestrictor n) = "!" ++ (show n)
  show (PosateRestrictor p) = show p
  
instance Show AndRestrictor where
  show (SimpleAndRestrictor n)   = show n
  show (ProperAndRestrictor a n) = (show a) ++ " & " ++ (show n)
  
instance Show OrRestrictor where
  show (SimpleOrRestrictor a)   = show a
  show (ProperOrRestrictor o a) = (show o) ++ " | " ++ (show a)

instance Show Cond where
  show (SimpleCond a)  = show a
  show (RestrCond a o) = (show a) ++ " @ " ++ (show o)

instance Eq Name where
  (Name a) == (Name b) = a == b
