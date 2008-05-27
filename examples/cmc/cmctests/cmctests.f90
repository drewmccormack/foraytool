program CMCTests
  use TestRunnerModule
  implicit none
  type (TestRunner) :: runner
  call New(runner)
  call run(runner)
  call Delete(runner)
end program

