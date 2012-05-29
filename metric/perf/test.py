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

    problems = ['randmat', 'thresh']
    main.inputs = ['a b c']
    main.input_thresh = ['thresh']
    main.input_winnow = ['winnow']
    main.write_to_file('randmat0.in', 'a b c\n')
    main.write_to_file('chain0.in', 'b\nc\nthresh\nwinnow\n')
    main.system('../../cpp/randmat/main < randmat0.in > randmat0.out')
    main.system('cp randmat0.out thresh0.in')
    main.append_to_file('thresh0.in', 'thresh\n')
    main.system('rm randmat0.out')

    m.ReplayAll()
    main.create_inputs(problems)
    m.VerifyAll()
    m.UnsetStubs()

if __name__ == '__main__':
  unittest.main()
