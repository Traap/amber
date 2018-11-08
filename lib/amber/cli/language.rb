# frozen_string_literal: true

# Languages that Amber support.
module Amber
  module Language
    NAMES = %w[zz cs da de en es fr-ca fr-eu it ne no pl ro sv].freeze

    CODE = { 'zz' => 'n/a',
             'cs' => 'Czech',
             'da' => 'Dansk',
             'de' => 'Deutsch',
             'en' => 'English',
             'es' => 'Espanol',
             'fr-ca' => 'CA French - Canadian',
             'fr-eu' => 'EU French - European',
             'it' => 'Italiano',
             'ne' => 'Nederlands',
             'no' => 'Norsk',
             'pl' => 'Polish',
             'ro' => 'Romanian',
             'sv' => 'Svenska' }.freeze

    DEFAULT = 'zz'.freeze
  end
end
