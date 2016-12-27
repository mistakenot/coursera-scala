package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("create simple code tree") {
    new TestTrees {
      val testStr = List('a', 'b')
      val tree = createCodeTree(testStr)
      assert(tree === t1)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times gives correct answer") {
    assert(times(List('a', 'b', 'a')) === List(('b', 1), ('a', 2)))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert to code table") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
    }
  }

  test("decode secret") {
    assert("huffmanestcool" === decode(frenchCode, secret).mkString)
  }

  test("quick decode decret") {
    val table = convert(frenchCode)

  }

  test("build a large tree") {
    val text = "Le calcul de la fréquence des lettres dans une langue est difficile et soumis à interprétation. On compte la fréquence des lettres d’un texte arbitrairement long, mais un certain nombre de paramètres influencent les résultats :\n\n    Le style narratif : s’il y a beaucoup de verbes à la 2e personne du pluriel (le vouvoiement, présent dans beaucoup de dialogues), il y aura significativement plus de « Z ».\n    Le vocabulaire spécifique du document : si l’on parle de chemins de fer, il y aura beaucoup plus de « W » (wagon) ; si l’un des protagonistes se dénomme Loïs, le nombre d'« Ï » s’en ressentira.\n    Le type de document : des petites annonces en France présenteront souvent le symbole Euro (€), qui est absent de la plupart des autres documents.\n    Les paramètres techniques : on peut facilement calculer des statistiques sur des textes informatisés, mais souvent ceux-ci ne comportent pas de majuscules accentuées (car difficiles à entrer sur certains ordinateurs) et il arrive aux auteurs d'oublier des accents. La graphie de l’e-dans-l’o (œ) est impossible à représenter dans le codage latin-1 qui est souvent utilisé pour les textes en français. C'est un problème parce que « œ » n'est pas une ligature esthétique (optionnelle) mais une ligature linguistique (obligatoire), elle se prononce différemment de la suite de voyelles « oe » . Par exemple, « œ » va se prononcer [ø] dans vœux alors que « oe » va se prononcer [ɔ.ɛ] dans coexistence.\n    La présence de caractères non alphabétiques (symboles de ponctuation, chiffres, parenthèses et accolades, symboles mathématiques courants…) peut ou non être prise en compte ; la virgule, le point ou l’apostrophe sont par exemple plus fréquents que plus de la moitié des lettres[réf. souhaitée].\n\nSi ces paramètres ont un impact spectaculaire sur les symboles les moins fréquents (la fréquence du œ varie entre 0,002 % et 0,09 % pour trois textes pris au hasard)[réf. nécessaire], elle est également sensible même pour les lettres les plus fréquentes (l’ordre de fréquence des lettres A, S, I, T et N, qui sont les plus fréquentes à part E, fluctue d’un texte à l’autre)."
    val tree = createCodeTree(text.toCharArray.toList)
  }

}
