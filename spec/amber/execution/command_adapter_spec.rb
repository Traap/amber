# frozen_string_literal: true

RSpec.describe Amber::Execution::CommandAdapter do
  let(:options) { Amber::Options.new }
  let(:step) do
    instance_double(
      Amber::TestStep,
      options: options,
      command: 'echo hello',
      workingdir: '/tmp'
    )
  end
  let(:status) { instance_double(Process::Status, success?: true) }

  it 'returns a normalized passed result for a successful command' do
    allow(Amber::TestEvidence).to receive(:run_from_temp_directory)
      .with('echo hello', '/tmp')
      .and_return(["hello\n", '', status])
    allow(options).to receive(:run?).and_return(true)

    result = described_class.new.execute(step)

    expect(result).to be_passed
    expect(result.stdout).to eq("hello\n")
    expect(result.metadata[:process_status]).to eq(status)
  end

  it 'returns skipped when the workflow is in dry-run mode' do
    allow(options).to receive(:run?).and_return(false)

    result = described_class.new.execute(step)

    expect(result).to be_skipped
  end
end
