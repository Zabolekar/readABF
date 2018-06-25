context("ABF 1.xx")

test_that("ABF 1.83 files can be loaded", {
   for (name in c("testProblemabfload.abf",
                  "testProblemabfload2.abf",
                  file.path("TestABF", "1 M KCl 10 mM HEPES pH 75_0051_porin.abf"))) {
      basic_test(
         name,
         "1.83",
         c("Im        ", "Vm        "),
         c("pA      ", "mV      "),
         c(3000000, 2)
      )
   }

   basic_test(
      file.path("TestABF", "1 M KCl 10 mM HEPES pH 75_0034_BLM_ 1 u 10_Gramicidin.abf"),
      "1.83",
      c("Im        ", "Vm        "),
      c("pA      ", "mV      "),
      c(600000, 2)
   )

   for (name in c(file.path("TestABF", "1 M KCl 10 mM HEPES pH 75_0033.abf"),
                  file.path("TestABF", "1 M KCl 10 mM HEPES pH 75_0092.abf"),
                  file.path("TestABF", "1 M KCl 10 mM HEPES pH 75_0113.abf"))) {
      basic_test(
         name,
         "1.83",
         c("IN 0      ", "10_Vm     "),
         c("pA      ", "mV      "),
         c(1032258, 2, 1)
      )
   }

})
