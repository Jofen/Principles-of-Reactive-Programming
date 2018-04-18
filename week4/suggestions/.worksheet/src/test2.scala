object test2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(62); 
  val fruits = Seq("apple", "banana", "orange");System.out.println("""fruits  : Seq[String] = """ + $show(fruits ));$skip(28); val res$0 = 
  fruits.map(_.toUpperCase);System.out.println("""res0: Seq[String] = """ + $show(res$0));$skip(32); val res$1 = 
  fruits.flatMap(_.toUpperCase);System.out.println("""res1: Seq[Char] = """ + $show(res$1));$skip(36); val res$2 = 
  fruits.map(_.toUpperCase).flatten;System.out.println("""res2: Seq[Char] = """ + $show(res$2));$skip(28); 
  
  val chars = 'a' to 'z';System.out.println("""chars  : scala.collection.immutable.NumericRange.Inclusive[Char] = """ + $show(chars ));$skip(117); val res$3 = 
  
  chars flatMap { a =>
    chars flatMap { b =>
      if (a != b) Seq("%c%c".format(a, b))
      else Seq()
    }
  };System.out.println("""res3: scala.collection.immutable.IndexedSeq[String] = """ + $show(res$3));$skip(91); val res$4 = 
  
  for {
    a <- chars
    b <- chars
    if (a != b)
  } yield "%c%c".format(a, b);System.out.println("""res4: scala.collection.immutable.IndexedSeq[String] = """ + $show(res$4))}

}
