# frozen_string_literal: true

# Languages that are supported.  The default is zz.
module Amber
  module Language
    CODE = { 'zz' => 'n/a',
             'cs' => 'Czech',
             'da' => 'Dansk',
             'de' => 'Deutsch',
             'en' => 'English',
             'en-US' => 'US English',  # variant found on the VMM
             'es' => 'Espanol',
             'fr' => 'French',  # variant found on the VMM
             'fr-ca' => 'CA French - Canadian',
             'fr-eu' => 'EU French - European',
             'hu' => 'Hungarian',
             'it' => 'Italiano',
             'nl' => 'Nederlands',  # variant found on the VMM. Code 'ne' found in IS-639-1 is Nepali.
             'no' => 'Norsk',
             'pl' => 'Polish',
             'pt' => 'Portuguese',
             'ro' => 'Romanian',
             'sk' => 'Slovak',
             'sv' => 'Svenska' }.freeze

    NAMES = CODE.keys.freeze  # Only CODE needs to be changed to update languages.

    DEFAULT = 'zz'.freeze
  end
end
