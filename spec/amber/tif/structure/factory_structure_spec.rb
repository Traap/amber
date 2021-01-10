require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate nesting a Test Plan, Test Suite, or Test Case YAML
# file.
# ------------------------------------------------------------------------------
describe 'Factory Structure' do
  describe 'Test Plan Name' do
    it 'is normal.' do
      f = Amber::FactoryStructure.plan_name('foo')
      expect(f).to eq('factory/plan/foo/foo.yaml')
    end

    it 'is nested.' do
      f = Amber::FactoryStructure.plan_name('foo/bar/baz')
      expect(f).to eq('factory/plan/foo/bar/baz/baz.yaml')
    end
  end

  describe 'Test Suite Name' do
    it 'is normal.' do
      f = Amber::FactoryStructure.suite_name('foo')
      expect(f).to eq('factory/suite/foo/foo.yaml')
    end

    it 'is nested.' do
      f = Amber::FactoryStructure.suite_name('foo/bar/baz')
      expect(f).to eq('factory/suite/foo/bar/baz/baz.yaml')
    end
  end

  describe 'Test Case Name' do
    it 'is normal.' do
      f = Amber::FactoryStructure.case_name('foo')
      expect(f).to eq('factory/case/foo/foo.yaml')
    end

    it 'is nested.' do
      f = Amber::FactoryStructure.case_name('foo/bar/baz')
      expect(f).to eq('factory/case/foo/bar/baz/baz.yaml')
    end
  end
end

