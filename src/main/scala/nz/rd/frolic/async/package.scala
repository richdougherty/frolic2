package nz.rd.frolic

package object async {
  type -->[-A,+B] = Continuation[A, B]
}
