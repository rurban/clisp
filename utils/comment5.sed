# This is a sed equivalent of the 'comment5' program.
# Requires GNU sed!
#
# Copyright (C) 2018 by Bruno Haible
# This is Free Software, distributed under the GNU GPL v2+
# See http://www.gnu.org/copyleft/gpl.html

# Replace '# ' comments outside of single-line /* */ comments.
s%^\(\([^/#]\|//*[^/*]\|//*[*]\([^*]\|[*][*]*[^*/]\)*[*]/\|##*[^# ]\)*\)# \(\|.*[^\\]\)\([\\]\{0,1\}\)$%\1/* \4 */\5%

# Hide multiline /* */ comments from this processing.
# Start with a line that contains an unclosed /* .
/[/][*]\([^*]\|[*][*]*[^*/]\)*[*]*$/{
  tu
  :u
  n
  # Continue if we have a line such as /* or */ /* .
  s%\(/[*]\([^*]\|[*][*]*[^*/]\)*[*]*\)$%\1%
  tu
  # Stop if we have a line such as */ .
  s%^\(\([^*]\|[*][*]*[^/*]\)*[*][*]*/\)%\1%
  tv
  # Otherwise continue.
  bu
  :v
  # Replace '# ' comment that starts after */ .
  s%^\(\([^*]\|[*][*]*[^/*]\)*[*][*]*/\([^/#]\|//*[^/*]\|//*[*]\([^*]\|[*][*]*[^*/]\)*[*]/\|##*[^# ]\)*\)# \(\|.*[^\\]\)\([\\]\{0,1\}\)$%\1/* \5 */\6%
}
