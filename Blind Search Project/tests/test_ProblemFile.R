source("../problem/towers-hanoi.r");
context("Testing Problem Code")


test_that('Problem has a name', {
  towerCount <- 3
  diskCount <- 5
  problem <- initialize.problem(towerCount, diskCount) 
  expect_true(nchar(problem$name) > 1)
})

test_that('Possible Actions has n(n-1) length', {
  towerCount <- 3
  diskCount <- 5
  problem <- initialize.problem(towerCount, diskCount) 
  expect_equal((towerCount*(towerCount-1)), nrow(problem$actions_possible))
})

test_that('Initial State has all disks in first tower', {
  towerCount <- 3
  diskCount <- 5
  problem <- initialize.problem(towerCount, diskCount) 
  expect_equal(sum(problem$state_initial), diskCount)
})

test_that("Final state has all disks in last tower", {
  towerCount <- 3
  diskCount <- 5
  problem <- initialize.problem(towerCount, diskCount) 
  expect_equal(sum(problem$state_final), towerCount*diskCount)
})

test_that("Can move a disk from one tower to an empty tower", {
  towerCount <- 3
  diskCount <- 5
  problem <- initialize.problem(towerCount, diskCount) 
  state <- effect(problem$state_initial, c(1,2))
  expect_identical(state, c(1,1,1,1,2))
})


test_that("Larger disk on smaller disk is not applicable", {
  towerCount <- 3
  diskCount <- 5
  problem <- initialize.problem(towerCount, diskCount) 
  state <- effect(problem$state_initial, c(1,2))
  expect_false(is.applicable(state, c(1,2), problem))
})

test_that("Can move smaller disk on larger disk", {
  towerCount <- 3
  diskCount <- 5
  problem <- initialize.problem(towerCount, diskCount) 
  state <- effect(problem$state_initial, c(1,2))
  state <- effect(state, c(1,3))
  state <- effect(state, c(2,3))
  expect_identical(state, c(1,1,1,3,3))
})

test_that("Smaller disk on larger disk is applicable", {
  towerCount <- 3
  diskCount <- 5
  problem <- initialize.problem(towerCount, diskCount) 
  state <- effect(problem$state_initial, c(1,2))
  state <- effect(state, c(1,3))
  expect_true(is.applicable(state, c(2,3)))
})

test_that("Final state returns correctly", {
  towerCount <- 3
  diskCount <- 4
  problem <- initialize.problem(towerCount, diskCount) 
  test_finalState <- c(3,3,3,3)
  test_notFinalState <- c(3,3,1,3)
  expect_true(is.final.state(test_finalState, problem$state_final))
  expect_false(is.final.state(test_notFinalState, problem$state_final))
})

test_that("Some example movements work", {
  towerCount <- 3
  diskCount <- 4
  problem <- initialize.problem(towerCount, diskCount) 
  expect_false(is.applicable(c(1,2,3,3), c(1,3)), problem)
  expect_identical(effect(c(1,2,3,3), c(1,3)), c(1,2,3,3))
  ex2_state <- c(1,2,3,3)
  ex2_action <- c(2,1)
  ex2_res <- c(1,1,3,3)
  expect_true(is.applicable(ex2_state, ex2_action, problem))
  expect_identical(effect(ex2_state, ex2_action), ex2_res)
  ex3_state <- c(3,3,3,2)
  ex3_action <-c(2,3)
  ex3_res <- c(3,3,3,3)
  expect_true(is.applicable(ex3_state, ex3_action, problem))
  expect_identical(effect(ex3_state, ex3_action), ex3_res)
})

