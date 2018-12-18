package restrictions

import higherkindness.droste.data.Fix

package object ast
  extends RestrictStatementInstances
  with ObservationInstances {

  abstract class WithSourceInfo(val sourceInfo: SourceInfo)
  type RestrictStatement = Fix[RestrictStatementF]
}
