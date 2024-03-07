module Benchmarks
    (
    Bench,
    gridBenchmark,
    brpBenchmark,
    zeroconfBenchmark,
    chainBenchmark,
    doubleChainBenchmark,
    haddadMonmegeBenchmark,
    cDrive2Benchmark,
    tireWorldBenchmark,
    ijBenchmark,
    ) where

import           ForMDPS

type Bench = (String, String, String, DeltaS Rational, [Rational])

--
-- grid.pm
--
deltaGrid :: Fractional a => (Int, Int) -> DeltaS a
deltaGrid (grid_n, grid_m) =
  DeltaS {
    vars = [("a", (0, grid_n), 0), ("b", (0, grid_m), 0)],
    delta = [(And [Le (V "a") (C grid_n), Le (V "b") (C grid_m)], [(0.7, [("a", Add [V "a", C 1])]), (0.3, [("b", Add [V "b", C 1])])])],
    bad = And [Eq (V "b") (C grid_m), Le (V "a") (C grid_n)]
  }


grid_nm = (10, 10)
grid_nms = (32, 32)
gridBenchmark :: [Bench]
gridBenchmark = [
  ("Grid", "grid.pm", "0.0012", deltaGrid grid_nm, [0.3, 0.2]),
  ("Grid", "grid_small.pm", "small", deltaGrid grid_nms, [0.3, 0.2])
  ]

--
-- brp_small.pm
--

(to_send, maxx, package_size) = (15, 8, 7)

deltaBRPS :: Fractional a => DeltaS a
deltaBRPS =
  DeltaS {
    vars = [("sent", (0, to_send), 0), ("failed", (0, maxx), 0), ("cur_package", (0, package_size), 0)],
    delta = [
      (And [Eq (V "cur_package") (C package_size), Le (V "sent") (C to_send)], [(1, [("failed", C 0), ("sent", Add [V "sent", C 1]), ("cur_package", C 0)])]),
      (And [Le (V "cur_package") (C package_size), Le (V "sent") (C to_send)], [(0.2, [("failed", Add [V "failed", C 1])]), (0.8, [("cur_package", Add [V "cur_package", C 1])])])
    ],
    bad = Eq (V "failed") (C maxx)
  }

brpBenchmark :: [Bench]
brpBenchmark = [("BRP", "brp_small.pm", "0.035", deltaBRPS, [0.1, 0.01, 0.005])]

--
-- zero_conf(_medium, _gigantic).pm
--


deltaZC :: Fractional a => Int -> DeltaS a
deltaZC num_probes =
  DeltaS {
    vars = [("s", (0, 1), 1), ("e", (0, 1), 0), ("c", (0, num_probes), 0)],
    delta = [
        (And [Eq (V "s") (C 1), Eq (V "e") (C 0)], [(0.5, [("s", C 0)]), (0.5, [("s", C 0), ("e", C 1)])]),
        (And [Eq (V "s") (C 0), Eq (V "e") (C 0), Le (V "c") (C num_probes)], [(0.999999999, [("c", Add [V "c", C 1])]), (1 - 0.999999999, [("s", C 1), ("c", C 0)])])
    ],
    bad = Eq (V "e") (C 1)
    }

num_probes_s = 100
num_probes_m = 10000
num_probes_g2 = 1000000000

zeroconfBenchmark :: [Bench]
zeroconfBenchmark = [
  ("ZeroConf", "zero_conf_small.pm", "0.5", deltaZC num_probes_s, [0.9, 0.75, 0.52, 0.45]),
  ("ZeroConf", "zero_conf_medium.pm", "0.5", deltaZC num_probes_m, [0.9, 0.75, 0.52, 0.45])
  ]

--
-- chain(_small, _medium, _gigantic).pm
--

deltaC :: Fractional a => Int -> a -> DeltaS a
deltaC nc p =
  DeltaS {
    vars = [("c", (0, nc), 0), ("g", (0, 1), 0)],
    delta = [(Le (V "c") (C nc), [(p, [("g", C 1)]), (1-p, [("c", Add [V "c", C 1])])])],
    bad = Eq (V "g") (C 1)
  }

(n_cs, p_cs) = (500, 0.001) -- for chain_small.pm
(n_cm, p_cm) = (5000, 0.0001) -- for chain_medium.pm
(n_cg, p_cg) = (50000000*10000, 0.000000000001) -- for chain_gigantic.pm

chainBenchmark :: [Bench]
chainBenchmark = [
  ("Chain", "chain_small.pm", "0.394", deltaC n_cs p_cs, [0.9, 0.4, 0.35, 0.3])
  -- ("Chain", "chain_medium.pm", "0.394", deltaC n_cm p_cm, [0.9, 0.48, 0.4, 0.3]),
  -- ("Chain", "chain_gigantic.pm", "0.394", deltaC n_cg p_cg, [0.9, 0.4])
  ]

--
-- double_chain(_small, _medium, _gigantic).pm
--

deltaDC :: Fractional a => Int -> a -> DeltaS a
deltaDC nd p =
  DeltaS {
    vars = [("c", (0, nd), 0), ("f", (0, 1), 0), ("g", (0, 1), 0)],
    delta = [
      (And [Le (V "c") (C nd), Eq (V "f") (C 0)], [(0.1, [("f", C 1)]), (0.0001, [("g", C 1)]), (0.8999, [("c", Add [V "c", C 1])])]),
      (And [Le (V "c") (C nd), Eq (V "f") (C 1)], [(p, [("g", C 1)]), (1-p, [("c", Add [V "c", C 1])])])
    ],
    bad = Eq (V "g") (C 1)
  }

(n_ds, p_ds) = (250, 0.001) -- for double_chain_small.pm
(n_dm, p_dm) = (2500, 0.0001) -- for double_chain_medium.pm
(n_dg, p_dg) = (2500000, 0.0000000001) -- for double_chain_gigantic.pm

doubleChainBenchmark :: [Bench]
doubleChainBenchmark = [
  ("DoubleChain", "double_chain_small.pm", "0.215", deltaDC n_ds p_ds, [0.9, 0.3, 0.216, 0.15])
  -- ("DoubleChain", "double_chain_medium.pm", "0.22", deltaDC n_dm p_dm, [0.9, 0.3, 0.24])
  ]

--
-- haddad_monmege.pm from https://qcomp.org/benchmarks/#haddad-monmege
--

deltaHM :: Fractional a => Int -> a -> DeltaS a
deltaHM n_large p =
  DeltaS {
    vars = [("x", (0, 2*n_large), n_large)],
    delta = [
      (Eq (V "x") (C n_large), [(p, [("x", C (n_large-1))]), (1-p, [("x", C (n_large+1))])]),
      (And [Le (C 0) (V "x"), Le (V "x") (C n_large)], [(0.5, [("x", Add [V "x", C (-1)])]), (0.5, [("x", C n_large)])]),
      (And [Le (C n_large) (V "x"), Le (V "x") (C $ 2*n_large)], [(0.5, [("x", Add [V "x", C 1])]), (0.5, [("x", C n_large)])]),
      (Or [Eq (V "x") (C 0), Eq (V "x") (C $ 2*n_large)], [(1, [])])
    ],
    bad = Eq (V "x") (C 0)
  }

n_hm_s = 20
n_hm_m = 500

haddadMonmegeBenchmark :: [Bench]
haddadMonmegeBenchmark = [
  ("HaddadMonmege", "small", "0.7", deltaHM n_hm_s 0.7, [0.9, 0.75]),
  ("HaddadMonmege", "medium", "0.7", deltaHM n_hm_m 0.7, [0.9, 0.75])
  ]

{-
According to the source https://qcomp.org/benchmarks/#cdrive, the optimal
probability is lambda: 0.8645657798255073
-}
cDrive2 :: DeltaS Rational
cDrive2 = DeltaS {
    vars = [("var0", (0, 2), 0), ("var1", (0, 2), 1), ("var2", (0, 4), 0), ("var3", (0, 2), 1), ("var4", (0, 4), 2), ("var5", (0, 2), 1), ("var6", (0, 2), 0)],
    delta =
        [ (And [And [Eq (V "var4") (C 2), Eq (V "var2") (C 0)], Eq (V "var0") (C 0)], [(0.9, [("var4", C 0)]), (0.1, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var2") (C 0)], Eq (V "var1") (C 0)], [(0.9, [("var4", C 0)]), (0.1, [("var4", C 1)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 2)], Eq (V "var2") (C 0)], [(0.9, [("var4", C 0)]), (0.1, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var3") (C 0)], Eq (V "var2") (C 0)], [(0.1, [("var4", C 0)]), (0.9, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var2") (C 1)], Eq (V "var0") (C 0)], [(0.1, [("var4", C 0)]), (0.9, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var2") (C 1)], Eq (V "var1") (C 0)], [(0.1, [("var4", C 0)]), (0.9, [("var4", C 1)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 2)], Eq (V "var2") (C 1)], [(0.1, [("var4", C 0)]), (0.9, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var3") (C 0)], Eq (V "var2") (C 1)], [(0.9, [("var4", C 0)]), (0.1, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var2") (C 2)], Eq (V "var0") (C 0)], [(0.1, [("var4", C 0)]), (0.9, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var2") (C 2)], Eq (V "var1") (C 0)], [(0.1, [("var4", C 0)]), (0.9, [("var4", C 1)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 2)], Eq (V "var2") (C 2)], [(0.1, [("var4", C 0)]), (0.9, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var3") (C 0)], Eq (V "var2") (C 2)], [(0.9, [("var4", C 3)]), (0.1, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var2") (C 3)], Eq (V "var0") (C 0)], [(0.9, [("var4", C 0)]), (0.1, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var2") (C 3)], Eq (V "var1") (C 0)], [(0.9, [("var4", C 0)]), (0.1, [("var4", C 1)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 2)], Eq (V "var2") (C 3)], [(0.9, [("var4", C 0)]), (0.1, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 2), Eq (V "var3") (C 0)], Eq (V "var2") (C 3)], [(0.1, [("var4", C 0)]), (0.9, [("var4", C 1)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 0)], Eq (V "var0") (C 0)], [(0.1, [("var0", C 1), ("var1", C 0), ("var2", C 1), ("var4", C 2), ("var6", C 1)]), (0.9, [("var0", C 1), ("var1", C 0), ("var2", C 1), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 3)], Eq (V "var1") (C 0)], [(0.1, [("var0", C 0), ("var1", C 1), ("var2", C 2), ("var4", C 2), ("var6", C 1)]), (0.9, [("var0", C 0), ("var1", C 1), ("var2", C 2), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 2)], Eq (V "var1") (C 0)], [(0.1, [("var1", C 1), ("var2", C 0), ("var4", C 2), ("var5", C 0), ("var6", C 1)]), (0.9, [("var1", C 1), ("var2", C 0), ("var4", C 2), ("var5", C 0)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var3") (C 0)], Eq (V "var2") (C 0)], [(0.1, [("var2", C 1), ("var3", C 1), ("var4", C 2), ("var5", C 0), ("var6", C 1)]), (0.9, [("var2", C 1), ("var3", C 1), ("var4", C 2), ("var5", C 0)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 0)], Eq (V "var2") (C 1)], [(0.1, [("var1", C 0), ("var2", C 3), ("var4", C 2), ("var5", C 1), ("var6", C 1)]), (0.9, [("var1", C 0), ("var2", C 3), ("var4", C 2), ("var5", C 1)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 0)], Eq (V "var2") (C 3)], [(0.1, [("var2", C 2), ("var3", C 0), ("var4", C 2), ("var5", C 1), ("var6", C 1)]), (0.9, [("var2", C 2), ("var3", C 0), ("var4", C 2), ("var5", C 1)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 3)], Eq (V "var0") (C 0)], [(0.1, [("var0", C 1), ("var1", C 0), ("var2", C 1), ("var4", C 2), ("var6", C 1)]), (0.9, [("var0", C 1), ("var1", C 0), ("var2", C 1), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 0)], Eq (V "var1") (C 0)], [(0.1, [("var0", C 0), ("var1", C 1), ("var2", C 2), ("var4", C 2), ("var6", C 1)]), (0.9, [("var0", C 0), ("var1", C 1), ("var2", C 2), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 1)], Eq (V "var1") (C 0)], [(0.1, [("var1", C 1), ("var2", C 0), ("var4", C 2), ("var5", C 0), ("var6", C 1)]), (0.9, [("var1", C 1), ("var2", C 0), ("var4", C 2), ("var5", C 0)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var3") (C 0)], Eq (V "var2") (C 3)], [(0.1, [("var2", C 1), ("var3", C 1), ("var4", C 2), ("var5", C 0), ("var6", C 1)]), (0.9, [("var2", C 1), ("var3", C 1), ("var4", C 2), ("var5", C 0)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 0)], Eq (V "var2") (C 2)], [(0.1, [("var1", C 0), ("var2", C 3), ("var4", C 2), ("var5", C 1), ("var6", C 1)]), (0.9, [("var1", C 0), ("var2", C 3), ("var4", C 2), ("var5", C 1)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 0)], Eq (V "var2") (C 0)], [(0.1, [("var2", C 2), ("var3", C 0), ("var4", C 2), ("var5", C 1), ("var6", C 1)]), (0.9, [("var2", C 2), ("var3", C 0), ("var4", C 2), ("var5", C 1)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 2)], Eq (V "var0") (C 0)], [(0.02, [("var0", C 1), ("var2", C 0), ("var3", C 0), ("var4", C 2), ("var6", C 1)]), (0.98, [("var0", C 1), ("var2", C 0), ("var3", C 0), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var3") (C 0)], Eq (V "var2") (C 1)], [(0.02, [("var0", C 0), ("var2", C 3), ("var3", C 1), ("var4", C 2), ("var6", C 1)]), (0.98, [("var0", C 0), ("var2", C 3), ("var3", C 1), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 1)], Eq (V "var0") (C 0)], [(0.02, [("var0", C 1), ("var2", C 0), ("var3", C 0), ("var4", C 2), ("var6", C 1)]), (0.98, [("var0", C 1), ("var2", C 0), ("var3", C 0), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var3") (C 0)], Eq (V "var2") (C 2)], [(0.02, [("var0", C 0), ("var2", C 3), ("var3", C 1), ("var4", C 2), ("var6", C 1)]), (0.98, [("var0", C 0), ("var2", C 3), ("var3", C 1), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 1)], Eq (V "var0") (C 0)], [(0.1, [("var0", C 1), ("var1", C 0), ("var4", C 2), ("var6", C 1)]), (0.9, [("var0", C 1), ("var1", C 0), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 2)], Eq (V "var1") (C 0)], [(0.1, [("var0", C 0), ("var1", C 1), ("var4", C 2), ("var6", C 1)]), (0.9, [("var0", C 0), ("var1", C 1), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 0)], Eq (V "var1") (C 0)], [(0.1, [("var1", C 1), ("var4", C 2), ("var5", C 0), ("var6", C 1)]), (0.9, [("var1", C 1), ("var4", C 2), ("var5", C 0)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var3") (C 0)], Eq (V "var2") (C 1)], [(0.1, [("var3", C 1), ("var4", C 2), ("var5", C 0), ("var6", C 1)]), (0.9, [("var3", C 1), ("var4", C 2), ("var5", C 0)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 0)], Eq (V "var2") (C 3)], [(0.1, [("var1", C 0), ("var4", C 2), ("var5", C 1), ("var6", C 1)]), (0.9, [("var1", C 0), ("var4", C 2), ("var5", C 1)])])
        , (And [And [Eq (V "var5") (C 0), Eq (V "var4") (C 0)], Eq (V "var2") (C 2)], [(0.1, [("var3", C 0), ("var4", C 2), ("var5", C 1), ("var6", C 1)]), (0.9, [("var3", C 0), ("var4", C 2), ("var5", C 1)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var2") (C 0)], Eq (V "var0") (C 0)], [(0.02, [("var0", C 1), ("var3", C 0), ("var4", C 2), ("var6", C 1)]), (0.98, [("var0", C 1), ("var3", C 0), ("var4", C 2)])])
        , (And [And [Eq (V "var4") (C 0), Eq (V "var3") (C 0)], Eq (V "var2") (C 3)], [(0.02, [("var0", C 0), ("var3", C 1), ("var4", C 2), ("var6", C 1)]), (0.98, [("var0", C 0), ("var3", C 1), ("var4", C 2)])])
        , (And [Eq (V "var4") (C 1), Eq (V "var0") (C 0)], [(0.001, [("var4", C 0), ("var6", C 1)]), (0.099, [("var4", C 0)]), (0.009, [("var6", C 1)]), (0.891, [])])
        , (And [Eq (V "var4") (C 1), Eq (V "var3") (C 0)], [(0.001, [("var4", C 0), ("var6", C 1)]), (0.099, [("var4", C 0)]), (0.009, [("var6", C 1)]), (0.891, [])])
        , (And [Eq (V "var5") (C 0), Eq (V "var4") (C 1)], [(0.002, [("var4", C 0), ("var6", C 1)]), (0.198, [("var4", C 0)]), (0.008, [("var6", C 1)]), (0.792, [])])
        , (And [Eq (V "var4") (C 1), Eq (V "var0") (C 0)], [(0.005, [("var4", C 0), ("var6", C 1)]), (0.495, [("var4", C 0)]), (0.005, [("var6", C 1)]), (0.495, [])])
        , (And [Eq (V "var4") (C 1), Eq (V "var3") (C 0)], [(0.005, [("var4", C 0), ("var6", C 1)]), (0.495, [("var4", C 0)]), (0.005, [("var6", C 1)]), (0.495, [])])
        ],
    -- the property in the jani file should be equivalent to the following PRISM property
    -- filter(min, Pmax=? [true U ("var6" = 0 ∧ "var5" = 0)], initial)
    -- However, since there is just one initial state (?), this should be equivalent to the following bad:
    bad = And [ Eq (V "var6") (C 0), Eq (V "var5") (C 0) ]
}

cDrive2Benchmark :: [Bench]
cDrive2Benchmark = [
  ("CDrive2", "", "~0.865", cDrive2, [0.9, 0.75, 0.5])
  ]

-- pretty big example, 8670 states
tireWorld :: DeltaS Rational
tireWorld = DeltaS {
    vars = [("var0", (0, 2), 0), ("var1", (0, 2), 0), ("var2", (0, 2), 0), ("var3", (0, 2), 0), ("var4", (0, 2), 0), ("var5", (0, 2), 0), ("var6", (0, 2), 0), ("var7", (0, 2), 1), ("var8", (0, 2), 0), ("var9", (0, 17), 9)],
    delta =
        [ (Eq (V "var7") (C 0), [(0.5, [("var7", C 1), ("var8", C 0)]), (0.5, [])])
        , (And [Eq (V "var9") (C 2), Eq (V "var0") (C 0)], [(1.0, [("var0", C 1), ("var7", C 0)])])
        , (And [Eq (V "var9") (C 4), Eq (V "var1") (C 0)], [(1.0, [("var1", C 1), ("var7", C 0)])])
        , (And [Eq (V "var9") (C 8), Eq (V "var2") (C 0)], [(1.0, [("var2", C 1), ("var7", C 0)])])
        , (And [Eq (V "var9") (C 11), Eq (V "var3") (C 0)], [(1.0, [("var3", C 1), ("var7", C 0)])])
        , (And [Eq (V "var9") (C 12), Eq (V "var4") (C 0)], [(1.0, [("var4", C 1), ("var7", C 0)])])
        , (And [Eq (V "var9") (C 14), Eq (V "var5") (C 0)], [(1.0, [("var5", C 1), ("var7", C 0)])])
        , (And [Eq (V "var9") (C 15), Eq (V "var6") (C 0)], [(1.0, [("var6", C 1), ("var7", C 0)])])
        , (And [Eq (V "var9") (C 0), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 4)]), (0.6, [("var9", C 4)])])
        , (And [Eq (V "var9") (C 0), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 8)]), (0.6, [("var9", C 8)])])
        , (And [Eq (V "var9") (C 1), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 9)]), (0.6, [("var9", C 9)])])
        , (And [Eq (V "var9") (C 1), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 10)]), (0.6, [("var9", C 10)])])
        , (And [Eq (V "var9") (C 2), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 4)]), (0.6, [("var9", C 4)])])
        , (And [Eq (V "var9") (C 2), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 5)]), (0.6, [("var9", C 5)])])
        , (And [Eq (V "var9") (C 2), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 12)]), (0.6, [("var9", C 12)])])
        , (And [Eq (V "var9") (C 3), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 8)]), (0.6, [("var9", C 8)])])
        , (And [Eq (V "var9") (C 4), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 0)]), (0.6, [("var9", C 0)])])
        , (And [Eq (V "var9") (C 4), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 2)]), (0.6, [("var9", C 2)])])
        , (And [Eq (V "var9") (C 4), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 8)]), (0.6, [("var9", C 8)])])
        , (And [Eq (V "var9") (C 4), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 16)]), (0.6, [("var9", C 16)])])
        , (And [Eq (V "var9") (C 5), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 2)]), (0.6, [("var9", C 2)])])
        , (And [Eq (V "var9") (C 5), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 7)]), (0.6, [("var9", C 7)])])
        , (And [Eq (V "var9") (C 5), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 10)]), (0.6, [("var9", C 10)])])
        , (And [Eq (V "var9") (C 5), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 14)]), (0.6, [("var9", C 14)])])
        , (And [Eq (V "var9") (C 6), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 8)]), (0.6, [("var9", C 8)])])
        , (And [Eq (V "var9") (C 6), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 10)]), (0.6, [("var9", C 10)])])
        , (And [Eq (V "var9") (C 6), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 13)]), (0.6, [("var9", C 13)])])
        , (And [Eq (V "var9") (C 7), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 5)]), (0.6, [("var9", C 5)])])
        , (And [Eq (V "var9") (C 8), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 0)]), (0.6, [("var9", C 0)])])
        , (And [Eq (V "var9") (C 8), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 3)]), (0.6, [("var9", C 3)])])
        , (And [Eq (V "var9") (C 8), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 4)]), (0.6, [("var9", C 4)])])
        , (And [Eq (V "var9") (C 8), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 6)]), (0.6, [("var9", C 6)])])
        , (And [Eq (V "var9") (C 8), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 12)]), (0.6, [("var9", C 12)])])
        , (And [Eq (V "var9") (C 8), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 16)]), (0.6, [("var9", C 16)])])
        , (And [Eq (V "var9") (C 9), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 1)]), (0.6, [("var9", C 1)])])
        , (And [Eq (V "var9") (C 10), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 1)]), (0.6, [("var9", C 1)])])
        , (And [Eq (V "var9") (C 10), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 5)]), (0.6, [("var9", C 5)])])
        , (And [Eq (V "var9") (C 10), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 6)]), (0.6, [("var9", C 6)])])
        , (And [Eq (V "var9") (C 10), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 11)]), (0.6, [("var9", C 11)])])
        , (And [Eq (V "var9") (C 11), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 10)]), (0.6, [("var9", C 10)])])
        , (And [Eq (V "var9") (C 12), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 2)]), (0.6, [("var9", C 2)])])
        , (And [Eq (V "var9") (C 12), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 8)]), (0.6, [("var9", C 8)])])
        , (And [Eq (V "var9") (C 12), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 15)]), (0.6, [("var9", C 15)])])
        , (And [Eq (V "var9") (C 13), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 6)]), (0.6, [("var9", C 6)])])
        , (And [Eq (V "var9") (C 14), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 5)]), (0.6, [("var9", C 5)])])
        , (And [Eq (V "var9") (C 14), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 16)]), (0.6, [("var9", C 16)])])
        , (And [Eq (V "var9") (C 15), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 12)]), (0.6, [("var9", C 12)])])
        , (And [Eq (V "var9") (C 15), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 16)]), (0.6, [("var9", C 16)])])
        , (And [Eq (V "var9") (C 16), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 4)]), (0.6, [("var9", C 4)])])
        , (And [Eq (V "var9") (C 16), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 8)]), (0.6, [("var9", C 8)])])
        , (And [Eq (V "var9") (C 16), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 14)]), (0.6, [("var9", C 14)])])
        , (And [Eq (V "var9") (C 16), Eq (V "var8") (C 0)], [(0.4, [("var8", C 1), ("var9", C 15)]), (0.6, [("var9", C 15)])])
        ],
    -- target probability: 0.23328
    bad = Eq (V "var9") (C 0)
}

tireWorldBenchmark :: [Bench]
tireWorldBenchmark = [
  ("TireWorld", "", "0.233", tireWorld, [0.9, 0.75, 0.5, 0.2])
  ]


--
-- ij.pm from https://qcomp.org/benchmarks/#ij
--

deltaIJ :: Fractional a => DeltaS a
deltaIJ =
  DeltaS {
    vars = [("q1", (0, 1), 1), ("q2", (0, 1), 1), ("q3", (0, 1), 1)],
    delta = [
      (Eq (V "q1") (C 1), [(0.5, [("q1", C 0), ("q3", C 1)]), (0.5, [("q1", C 0), ("q2", C 1)])])
    ],
    bad = Or [
      And [Eq (V "q1") (V "q2"), Not (Eq (V "q2") (V "q3"))],
      And [Eq (V "q2") (V "q3"), Not (Eq (V "q3") (V "q1"))],
      And [Eq (V "q3") (V "q1"), Not (Eq (V "q1") (V "q2"))]
      ]
  }

ijBenchmark :: [Bench]
ijBenchmark = [
  ("ij", "small", "1", deltaIJ, [1.0, 0.5])
  ]

-- --
-- -- consensus.2.v1.pm from https://qcomp.org/benchmarks/#consensus
-- --

-- deltaCS :: Fractional a => Int -> DeltaS a
-- deltaCS k =
--   DeltaS {
--     vars = [("pc1", (0, 3), 1), ("q2", (0, 1), 1), ("q3", (0, 1), 1)],
--     delta = [
--       (Eq (V "q1") (C 1), [(0.5, [("q1", C 0), ("q3", C 1)]), (0.5, [("q1", C 0), ("q2", C 1)])])
--     ],
--     bad = Eq (Add [V "q1", V "q2", V "q3"]) (C 1)
--   }

-- ijBenchmark :: [Bench]
-- ijBenchmark = [
--   ("ij", "small", "1", deltaIJ, [1.0, 0.5]),
--   ]