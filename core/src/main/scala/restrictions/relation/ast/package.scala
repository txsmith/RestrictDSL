package restrictions.relation

package object ast extends RelationDSLInstances {
  type RowUpdate[K,+V] = restrictions.domain.external.RowUpdate[K,V]
  val Delete = restrictions.domain.external.Delete
  val Update = restrictions.domain.external.Update
  val RowUpdate = restrictions.domain.external.RowUpdate
}
