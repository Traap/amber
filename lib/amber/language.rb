# Languages that Amber support.
module Amber
  module Language
    Names = %w[zz da de en es fr-ca fr-eu it ne no sv].freeze

    Code = { 'zz' => 'n/a',
             'da' => 'Dansk',
             'de' => 'Deutsch',
             'en' => 'English',
             'es' => 'Espanol',
             'fr-ca' => 'CA French - Canadian',
             'fr-eu' => 'EU French - European',
             'it' => 'Italiano',
             'ne' => 'Nederlands',
             'no' => 'Norsk',
             'sv' => 'Svenska' }.freeze

    Default = 'zz'.freeze
  end
end
