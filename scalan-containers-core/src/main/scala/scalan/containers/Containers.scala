package scalan.containers

import scalan._
import scalan.collections.{CollectionsDsl, CollectionsDslExp, CollectionsDslStd}
import scalan.collections.{CollectionsDsl, CollectionsDslExp, CollectionsDslStd}
import scalan.shaping._

trait ContainersDsl
  extends CollectionsDsl
     with CollectionContainers
     with ArrayContainers
     with MMapContainers {

  implicit override val collectionContainer =
    new CollectionTabular with CollectionEnumarable {
      def cI = this
      def cC = this
    }

  override implicit val arrayContainer: ArrayTabular with ArrayEnumarable =
    new ArrayTabular with ArrayEnumarable {
      def cI = this
      def cC = this
    }

  implicit def mmapContainer[K:Elem]: MMapTabular[K] = new MMapTabular[K] {
    def eK = element[K]
    def cI = collectionContainer
    def cC = collectionContainer
  }

}

trait ContainersDslStd
  extends CollectionsDslStd
     with ContainersDsl
     with ShapingDslStd
{ }

trait ContainersDslExp
  extends CollectionsDslExp
  with ContainersDsl
  with ShapingDslExp
{ }

