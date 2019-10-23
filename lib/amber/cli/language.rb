# frozen_string_literal: true

# Languages that are supported.  The default is zz.
module Amber
  module Language
    CODE = { 'zz' => 'n/a',
             'cs' => 'Czech',
             'da' => 'Dansk',
             'de' => 'Deutsch',
             'du' => 'Dutch',
             'en' => 'English',
             'en-US' => 'US English',
             'es' => 'Espanol',
             'fi' => 'Finnish',
             'fi' => 'Finish',
             'fr' => 'French',
             'ga' => 'Gaeilge',
             'hu' => 'Hungarian',
             'it' => 'Italiano',
             'ne' => 'Nederlands',
             'nl' => 'Nederlands',
             'no' => 'Norsk',
             'pl' => 'Polish',
             'pt' => 'Portuguese',
             'ro' => 'Romanian',
             'ru' => 'Russian',
             'sk' => 'Slovak',
             'sv' => 'Svenska' }.freeze

    # Only CODE needs to be changed to update languages.
    NAMES = CODE.keys.freeze

    # Immutable objects are not frozen.
    DEFAULT = 'zz'
  end
end
