# frozen_string_literal: true

RSpec.describe 'Amber browser integration', browser: true do
  %w[Chrome Brave Edge Firefox].each do |browser_name|
    it "starts #{browser_name}, loads a local page, and shuts down" do
      browser = Amber::Execution::BrowserFactory.new.start(browser_name)
      browser.goto('data:text/html,<title>Amber browser fixture</title><h1>Amber</h1>')

      expect(browser.title).to eq('Amber browser fixture')
      expect(browser.h1.text).to eq('Amber')
    ensure
      browser&.quit
    end
  end
end
