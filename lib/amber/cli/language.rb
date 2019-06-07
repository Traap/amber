# frozen_string_literal: true

# Languages that are supported.  The default is zz.
module Amber
  module Language
    NAMES = %w[zz cs da de en en-US es fr-ca fr-eu hu it ne no pl pt ro sk sv].freeze

    CODE = { 'zz' => 'n/a',
             'cs' => 'Czech',
             'da' => 'Dansk',
             'de' => 'Deutsch',
             'en' => 'English',
             'en-US' => 'US English',
             'es' => 'Espanol',
             'fr-ca' => 'CA French - Canadian',
             'fr-eu' => 'EU French - European',
             'hu' => 'Hungarian',
             'it' => 'Italiano',
             'ne' => 'Nederlands',
             'no' => 'Norsk',
             'pl' => 'Polish',
             'pt' => 'Portuguese',
             'ro' => 'Romanian',
             'sk' => 'Slovak',
             'sv' => 'Svenska' }.freeze

    DEFAULT = 'zz'.freeze
  end
end
