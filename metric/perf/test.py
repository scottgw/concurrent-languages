import unittest
import mox
import __builtin__
import main

class testMain(unittest.TestCase):
  def testGenerateErlangMain(self):
    m = mox.Mox()

    m.StubOutWithMock(main, 'write_to_file')
    main.write_to_file('directory/main.sh', 'dir=directory')

    m.StubOutWithMock(main, 'get_directory')
    main.get_directory('erlang', 'problem', 'variation').AndReturn(
        'directory')

    m.StubOutWithMock(main, 'get_problems_with_variations')
    main.get_problems_with_variations().AndReturn(
        [('problem', 'variation')])

    main.ERLANG_MAIN = 'dir=%s'

    m.ReplayAll()
    main.generate_erlang_main()
    m.VerifyAll()
    m.UnsetStubs()

  def testMakeAll(self):
    m = mox.Mox()

    m.StubOutWithMock(main, 'system')
    main.system('cd ../../language/problem/variation && make main')

    m.StubOutWithMock(main, 'get_all')
    main.get_all().AndReturn([('language', 'problem', 'variation')])

    m.ReplayAll()
    main.make_all()
    m.VerifyAll()
    m.UnsetStubs()

  def testCreateInputs(self):
    m = mox.Mox()

    m.StubOutWithMock(main, 'system')
    m.StubOutWithMock(main, 'write_to_file')
    m.StubOutWithMock(main, 'append_to_file')
    m.StubOutWithMock(main, 'read_from_file_skipping_first_line')
    m.StubOutWithMock(main, 'file_exists')
    main.file_exists('randmat_10_15_20.out').AndReturn(False)
    main.file_exists('thresh_10_15_30.in').AndReturn(False)
    main.file_exists('thresh_10_15_30.out').AndReturn(False)
    main.file_exists('winnow_10_15_40.in').AndReturn(False)
    main.file_exists('winnow_10_15_40.out').AndReturn(False)
    main.file_exists('outer_40.in').AndReturn(False)
    main.file_exists('outer_40.out').AndReturn(False)
    main.file_exists('product_40.in').AndReturn(False)
    main.file_exists('product_40.out').AndReturn(False)

    m.StubOutWithMock(main, 'get_directory')
    main.get_directory('cpp', 'randmat').AndReturn('dir_randmat')
    main.get_directory('cpp', 'thresh').AndReturn('dir_thresh')
    main.get_directory('cpp', 'winnow').AndReturn('dir_winnow')
    main.get_directory('cpp', 'outer').AndReturn('dir_outer')
    main.get_directory('cpp', 'product').AndReturn('dir_product')

    main.problem_classes = [main.RandmatProblem(), main.ThreshProblem(),
        main.WinnowProblem(), main.OuterProblem(), main.ProductProblem(),
        main.DumbProblem()]
    main.inputs = [main.ProblemInput(10, 15, 20, 30, 40)]
    main.write_to_file('randmat_10_15_20.in', '10 15 20\n')
    main.write_to_file('chain_10_20_30_40.in', '10\n20\n30\n40\n')
    main.system(
        'dir_randmat/main < randmat_10_15_20.in > randmat_10_15_20.out')
    main.system('cp randmat_10_15_20.out thresh_10_15_30.in')
    main.system(
        'dir_thresh/main < thresh_10_15_30.in > thresh_10_15_30.out')
    main.system('cp thresh_10_15_30.out winnow_10_15_40.in')
    main.append_to_file('thresh_10_15_30.in', '30\n')
    main.system('cp randmat_10_15_20.out winnow_10_15_40.in')
    main.read_from_file_skipping_first_line(
        'thresh_10_15_30.out').AndReturn('thresh_out')
    main.append_to_file('winnow_10_15_40.in', '\nthresh_out\n40\n')
    main.system(
        'dir_winnow/main < winnow_10_15_40.in > winnow_10_15_40.out')
    main.system('cp winnow_10_15_40.out outer_40.in')
    main.system(
        'dir_outer/main < outer_40.in > outer_40.out')
    main.system('cp outer_40.out product_40.in')
    main.system(
        'dir_product/main < product_40.in > product_40.out')

    m.ReplayAll()
    main.create_inputs()
    m.VerifyAll()
    m.UnsetStubs()

  def testCreateInputsFileExists(self):
    m = mox.Mox()

    m.StubOutWithMock(main, 'system')
    m.StubOutWithMock(main, 'write_to_file')
    m.StubOutWithMock(main, 'append_to_file')
    m.StubOutWithMock(main, 'read_from_file_skipping_first_line')
    m.StubOutWithMock(main, 'file_exists')
    main.file_exists('randmat_10_15_20.out').AndReturn(True)
    main.file_exists('thresh_10_15_30.in').AndReturn(True)
    main.file_exists('thresh_10_15_30.out').AndReturn(True)
    main.file_exists('winnow_10_15_40.in').AndReturn(True)
    main.file_exists('winnow_10_15_40.out').AndReturn(True)
    main.file_exists('outer_40.in').AndReturn(True)
    main.file_exists('outer_40.out').AndReturn(True)
    main.file_exists('product_40.in').AndReturn(True)
    main.file_exists('product_40.out').AndReturn(True)

    m.StubOutWithMock(main, 'get_directory')
    main.get_directory('cpp', 'randmat').AndReturn('dir_randmat')
    main.get_directory('cpp', 'thresh').AndReturn('dir_thresh')
    main.get_directory('cpp', 'winnow').AndReturn('dir_winnow')
    main.get_directory('cpp', 'outer').AndReturn('dir_outer')
    main.get_directory('cpp', 'product').AndReturn('dir_outer')

    main.problem_classes = [main.RandmatProblem(), main.ThreshProblem(),
        main.WinnowProblem(), main.OuterProblem(), main.ProductProblem(),
        main.DumbProblem()]
    main.inputs = [main.ProblemInput(10, 15, 20, 30, 40)]
    main.write_to_file('randmat_10_15_20.in', '10 15 20\n')
    main.write_to_file('chain_10_20_30_40.in', '10\n20\n30\n40\n')

    m.ReplayAll()
    main.create_inputs()
    m.VerifyAll()
    m.UnsetStubs()

  def testRunAll(self):
    m = mox.Mox()

    m.StubOutWithMock(main, 'system')
    m.StubOutWithMock(main, 'read_from_file')
    m.StubOutWithMock(main, 'get_all')

    m.StubOutWithMock(main, 'get_directory')
    main.get_directory('language', 'problem', 'variation').AndReturn(
        'directory')

    main.get_all().AndReturn([('language', 'problem', 'variation')])

    main.inputs = [main.ProblemInput(10, 15, 20, 30, 40)]
    main.system(('time -a -f %e -o time-language-problem-variation-0.out '
                 'directory/main < '
                 'problem_10_15_20_30_40.in > /dev/null 1>&0 2>&0'))
    main.read_from_file('time-language-problem-variation-0.out')

    m.ReplayAll()
    main.run_all()
    m.VerifyAll()
    m.UnsetStubs()

  def testGetResults(self):
    m = mox.Mox()

    m.StubOutWithMock(main, 'get_all')
    main.get_all().AndReturn([('language', 'problem', 'variation')])

    m.StubOutWithMock(main, 'read_file_values')
    main.read_file_values(
        'time-language-problem-variation-0.out').AndReturn([1, 2])

    m.ReplayAll()
    main.get_results()
    self.assertEqual(main.results['problem']['variation']['language'][0],
        (1 + 2) / 2.)
    m.VerifyAll()
    m.UnsetStubs()

if __name__ == '__main__':
  unittest.main()
