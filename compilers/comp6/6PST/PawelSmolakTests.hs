{-# LANGUAGE Safe #-}
module PawelSmolakTests(tests) where

import DataTypes

    -- Testy składające się z prostych programów
    -- testujące ogólną poprawność programu.
 ++ [
      Test { testName    = "roots_zero",
             testProgram = SrcFile "roots.pp5",
             testAnswer  = Eval [1, 0, 1] (Value 0)
           },

      Test { testName    = "roots_one",
             testProgram = SrcFile "roots.pp5",
             testAnswer  = Eval [1, 0, 0] (Value 1)
           },

      Test { testName    = "roots_two",
             testProgram = SrcFile "roots.pp5",
             testAnswer  = Eval [-2, 1, 2] (Value 2)
           },

      Test { testName    = "sum",
             testProgram = SrcFile "sum.pp5",
             testAnswer  = Eval [10, -5, -1, 0] (Value 13)
           },

      Test { testName    = "max",
             testProgram = SrcFile "max.pp5",
             testAnswer  = Eval [10, 3, 123] (Value 123)
           },

      Test { testName    = "weird",
             testProgram = SrcFile "weird.pp5",
             testAnswer  = Eval [1, 2, 3] RuntimeError
           }
    ]

