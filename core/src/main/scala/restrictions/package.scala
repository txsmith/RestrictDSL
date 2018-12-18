import restrictions.ast.{ObservationInstances, ObservationSyntax}
import restrictions.utils.{HFunctors, NamedLambdas}

import scala.language.higherKinds

package object restrictions
  extends ObservationSyntax
  with ObservationInstances
  with NamedLambdas
  with HFunctors

