package com.promindis.collections

trait MyIterator[T] {
  def next: T
  def hasNext: Boolean

  def foreach (op: T => Unit) {
    while (hasNext) op(next)
  }
}

trait MyBuilder[Container[_], T] {
  def += (item: T)
  def finalise(): Container[T]
}

trait MyBuildable[Container[_]] {

  def build[T]: MyBuilder[Container, T]

  def buildWith[T](f: MyBuilder[Container, T] => Unit) = {
    val builder = build[T]
    f(builder)
    builder
  }
}

trait MyIterable[T] {
  type Container[_] <: MyIterable[_]

  def elements : MyIterator[T]

  def mapTo[U, Container[_]](f: T => U)(buildable: MyBuildable[Container]) = {
    val buffer: MyBuilder[Container, U] = buildable.build[U]
    elements.foreach{ item =>
      buffer += f(item)
    }
    buffer.finalise()
  }

  def filterTo[Container[_]](p: (T) => Boolean)(buildable: MyBuildable[Container]) = {
    buildable.buildWith[T] { buffer =>
      elements.foreach{item =>
        if (p(item)) buffer += item}}.finalise()
  }

  def flatMapTo[U, Container[_]](f: T => Iterable[U])(buildable: MyBuildable[Container]) = {
    buildable.buildWith[U]{ buffer =>
      elements.foreach{ item =>
        f(item).foreach{ result =>
          buffer += result}}}.finalise()
  }

  def map[U](f: T => U)(buildable: MyBuildable[Container]):Container[U] = mapTo(f)(buildable)

  def filter(p: T => Boolean)(buildable: MyBuildable[Container]):Container[T] = filterTo(p)(buildable)

  def flatMap[U](f: T => Iterable[U])(buildable: MyBuildable[Container]): Container[U] = flatMapTo(f)(buildable)

}

//class MyList[T] extends MyIterable[T, MyList] {
//  def filter(p: (T) => Boolean) = null
//}