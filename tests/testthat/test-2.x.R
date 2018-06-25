context("ABF 2.xx")

test_that("ABF 2.00 files can be loaded", {
   for (name in c("testabfloadklappt.abf",
                  "testabfloadklappt2.abf",
                  "testabfloadklappt3.abf",
                  "testabfloadklappt4.abf",
                  file.path("TestABF", "110422_1 M KCl 10 mM HEPES DPhPC Chol tests _0018.abf"),
                  file.path("TestABF", "1 M KCl 10 mM HEPES DOPC Chol 7 3_0029.abf"))) {
      basic_test(
         name,
         "2.00",
         c("IN 0", "IN 1", "IN 2"),
         c("pA", "A", "mV"),
         c(1200000, 3)
      )
   }

   for (name in c(file.path("TestABF", "1 M KCl 10mM HEPES DPhPC Chol 9 1_0003.abf"),
                  file.path("TestABF", "1 M KCl 5 mM Hepes 1mM CaCl2 DPhPC Chol 9 1 _0006.abf"))) {
      basic_test(
         name,
         "2.00",
         c("IN 0", "IN 1", "IN 2"),
         c("pA", "A", "mV"),
         c(600000, 3)
      )
   }

   expect_warning(basic_test(
      file.path("TestABF", "+10mV.abf"),
      "2.00",
      "IN 0",
      "pA",
      c(239130, 1, 1)
   ), "problems in StringsSection")

   expect_warning(basic_test(
      file.path("TestABF", "-20mV_RGS_VDAC_50 sampling 1 filter.abf"),
      "2.00",
      "IN 0",
      "pA",
      c(358703, 1, 1)
   ), "problems in StringsSection")

   expect_warning(basic_test(
      "testProblemabfload3.abf",
      "2.00",
      c("Im", "Vm"),
      c("pA", "mV"),
      c(207987, 2)
   ), "problems in StringsSection")

   expect_warning(basic_test(
      "testProblemabfload4.abf",
      "2.00",
      c("Im", "Vm"),
      c("pA", "mV"),
      c(249434, 2)
   ), "problems in StringsSection")

})
