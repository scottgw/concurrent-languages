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

    m.StubOutWithMock(main, 'get_directory')
    main.get_directory('cpp', 'randmat').AndReturn('directory')

    problems = ['randmat', 'thresh']
    main.inputs = [main.ProblemInput(10, 15, 20, 30, 40)]
    main.input_thresh = ['thresh']
    main.input_winnow = ['winnow']
    main.write_to_file('randmat_10_15_20.in', '10 15 20\n')
    main.write_to_file('chain_10_20_30_40.in', '10\n20\n30\n40\n')
    main.system('directory/main < randmat0.in > randmat0.out')
    main.system('cp randmat0.out thresh0.in')
    main.append_to_file('thresh0.in', 'thresh\n')
    main.system('rm randmat0.out')

    m.ReplayAll()
    main.create_inputs(problems)
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

    main.system(('time -a -f %e -o time-language-problem-variation-0.out '
                 'directory/main < '
                 'problem0.in > /dev/null 1>&0 2>&0'))
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
