# This file contains basic examples of tests
# to be included in a file called by do.test.
# Run these examples using e.g.
#      do.test("testfile.t")
#      do.test("c:/myPath/testfile.t")


{ # This test should pass
  all.equal(24/8, 3)
} 

{ # This test should fail, and be printed
  all.equal(5, 6)
}

expectWarnings( { # example of using expectWarnings
  x <- data.frame(a=1:3,b=2:4)
  x[,3] <- x
  all.equal(ncol(x), 3)
}, expected = "provided 2 variables to replace 1 v")

expectWarnings(is.na(log(-1)), # another example
    expected = if(is.R()) "NaNs produced" else "NAs generated")

expectStop(lm(5), # example using expectStop
           expected = "invalid formula")

{ # final cleanup
  rm(x)
  TRUE
}
