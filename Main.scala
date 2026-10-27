import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/** 
 * 1. uzdevums
 * Fibonači skaitļu virknes aprēķināšana bez mainīgajiem.
 * 
 * Sākotnējā funkcija fibs, kas izmanto mainīgos tiek pārveidota
 * par funkciju fibs2, kas izmanto astes rekursiju un nemainīgas vērtības.
 * 
 * Skaitļu secība tiek saglabāta atbilstoši dotajam piemēram.
*/
def fibs(ct: Int): Seq[Int] = {
    var n1 = 0
    var n2 = 1

    for (i <- 1 to ct) yield {
        val x = n1 + n2
        n1 = n2
        n2 = x
        x
    }
}

def fibs2(ct: Int): Seq[Int] = {
    @scala.annotation.tailrec
    def loop(
        remaining: Int,
        n1: Int,
        n2: Int,
        acc: Vector[Int]
    ): Vector[Int] = {
        if (remaining <= 0) acc
        else {
            val x = n1 + n2
            loop(remaining - 1, n2, x, acc :+ x)
        }
    }

    loop(ct, 0, 1, Vector.empty[Int])
}

/** 
 * 2. uzdevums
 * Option un kolekciju apstrāde ar map un flatMap.
 * 
 * Tiek pārrakstīta for izteiksmi izamntojot tika flatMap un map,
 * lai iegūtu pieejamo adrešu pasta indeksus.
 * 
 * Tiek izmantots toSeq, lai apvienotu Option un secību apstrādi.
 * 
 * Tiek pārbuadīta tipu nesadrītva, kas rodas bez ārējās toSeq pārveidošanas.
*/
case class Address(
    street: String,
    city: String,
    state: String,
    zipCode: String
)

case class Person(
    first: String,
    last: String,
    address: Option[Address]
)

/**
 * 3. uzdevums
 * Funkcijas atgriešanas tipa noteikšana
 * 
 * Izmantojot kompilatrou noskaidro noteikto atgriešanas tipu,
 * ja viens if zars atgriež None, bet otrs - veselo skaitļu dalījumu.
*/
def safeDivOriginal(n: Int, d: Int) = 
    if (d == 0) None else n / d

/** 
 * 4. uzdevums
 * Skadra funkcijas atgriešanas tipa norādīšana
 * 
 * Norāda dalīšanas funkcijai atgriešanas tipu Option[Int].
 * Novērš tipu nesaderību, veiksmīgo dlījumu ietverot Some.
 * Ja dalītājs ir nulle - atgirež None.
*/
def safeDiv(n: Int, d: Int): Option[Int] = 
    if (d == 0) None else Some(n / d)

/** 
 * 5. uzdevums
 * Nosadījumu pārrakstīšana ar match.
 * 
 * Pārraksta funkciju describeSign, aizstājot if un else
 * ar match izsteiksmi un nosacījumu paraugā.
 * 
 * Saglabā rezultātus Zero, Positive un Negative
 * atbilstoši nullei, pozitīviem un negatīviem skaitļiem.
*/
def describeSign(n: Int): String = 
    if (n == 0) "Zero"
    else if (n > 0) "Positive"
    else "Negative"

def describeSignMatch(n: Int): String = 
    n match {
        case 0 => "Zero"
        case x if x > 0 => "Positive"
        case _ => "Negative"
    }

/**
 * 6. uzdevums
 * Asinhrona aprēķina izveide ar Future.
 * 
 * Pārveido skaitļu summas un gala teksta iegūšanau par
 * asinhronu aprēķinu kas atgriež Future[String].
 * 
 * Aprvieno Future rezultātus ar for izteikmi, neizmantojot'
 * bloķējošu gaidīšanu funkcijas iekšienē.
*/
def answerAsync(): Future[String] = {
    val numbersFuture = Future {
        (1, 2, 3)
    }

    val textFuture = Future {
        "The answer is"
    }

    for {
        numbers <- numbersFuture
        text <- textFuture
    } yield {
        val (a, b, c) = numbers
        val sum = a + b + c
        s"$text $sum"
    }
}


@main def lab3(): Unit = {

    val utf8Out = new java.io.PrintStream(System.out, true, "UTF-8")
    System.setOut(utf8Out)
       
    /** 
     * 1. uzdevums
     * Fibonači skaitļu rindas demonstrācija
     * 
     * Tiek attēloti sākotnējās un rekursīvās funkcijas rezultāti,
     * aprēķinot 10 Fibonači rindas elementus.
     * 
     * Tiek attēlota rekursīvās funkcijas darbība ar 0, 1 un 2 elementiem.
    */
    println("TASK 1: Generate Fibonacci numbers without mutable variables")
    println(s"Original function with var, 10 elements: ${fibs(10)}")
    println(s"Tail-recursive function, 10 elements: ${fibs2(10)}")
    println(s"Requested 0 elements: ${fibs2(0)}")
    println(s"Requested 1 element: ${fibs2(1)}")
    println(s"Requested 2 elements: ${fibs2(2)}")
    println()

    /** 
     * 2. uzdevums
     * 
     * Izveidoti piemēri cilvēkam bez adreses, cilvēkam ar adresi
     * un neesošam cilvēkam, izmantojot Some un None.
     * 
     * Salīdzina pasta indeksu iegūšanu ar for izteiksmi un tās ekvivalentu,
     * kas izmanto flatMap un map.
     * 
     * Pārbauda, ka rezultātā nonāk tikai pieejamie pasta indeksi.
    */
    println("TASK 2: Extract available postal codes using map and flatMap")

    val p1: Option[Person] = Some(Person("Fred", "Bloggs", None))

    val p2: Option[Person] = 
        Some(Person("Simon", "Jones",
        Some(Address("123 Main", "Fakesville", "AZ", "12345"))
    ))

    val p3: Option[Person] = None

    val zipCode = for {
        person <- p2
        address <- person.address
    } yield address.zipCode

    println(s"Single person, using for: $zipCode")

    val zipCodeWithMethods = p2.flatMap { person =>
        person.address.map { address =>
            address.zipCode
        }
    }

    println(s"Single person, using flatMap and map: $zipCodeWithMethods")

    val optPeople: Option[Seq[Option[Person]]] = 
        Some(Seq(p1, p2, p3))

    val zipCodes = for {
        people <- optPeople.toSeq
        personOpt <- people
        person <- personOpt.toSeq
        address <- person.address.toSeq
    } yield address.zipCode

    println(s"People collection, using for: $zipCodes")

    val zipCodesWithMethods = optPeople.toSeq.flatMap { people =>
        people.flatMap { personOpt =>
            personOpt.toSeq.flatMap { person =>
                person.address.toSeq.map { address =>
                    address.zipCode
                }
            }
        }
    }
    println(s"People collection, using flatMap and map: $zipCodesWithMethods")
    println()

    /** 
     * 3. uzdevums
     * 
     * Izsauc ārējo funkciju ar ne-nulles un nulles dalītāju.
     * 
     * Izvades rezultāti ir Int vērtība un None, nevis Some un None.
     * 
     * Funkcijas noteikto tipu pārbauda VS Code tipa informācijā.
    */
    
    println("TASK 3: Identify the inferred return type")
    println("Inferred return type of safeDivOriginal in Scala 3: None | Int")
    println(s"Division by a non-zero divisor, 6 / 2: ${safeDivOriginal(6, 2)}")
    println(s"Division by zero, 6 / 0: ${safeDivOriginal(6, 0)}")
    println()

    /** 
     * 4. uzdevums
     * 
     * Pārbauda dalīšanu ar nenulees un nulles dalītāju, kā arī
     * veselo skaitļu dalīšanas rezulātu.
     * 
     * Ar man iegūst sarakstu ar Option[Int] vērtībām.
     * Af flatten iegūst veiksmīgos rezultātu, izlaižot None
    */
    println("TASK 4: Return division results as Option[Int]")
    println(s"Successful division, 6 / 2: ${safeDiv(6, 2)}")
    println(s"Division by zero, 6 / 0: ${safeDiv(6, 0)}")
    println(s"Integer division, 5 / 2: ${safeDiv(5, 2)}")

    val divisionResults = List(0, 1, 2).map(d => safeDiv(5, d))
    println(s"Divide 5 by each divisor in List(0, 1, 2): $divisionResults")
    println(s"Available results after flatten: ${divisionResults.flatten}")
    println()

    /**
     * 5. uzdevums
     * 
     * Izsauc sākotnējo un ar match pārrakstītās funkcijas describeSign
     * negatīvam skaitlim, nullei un pozitīvam skaitlim.
     * 
     * Izdrukā rezultātus blakus lai pārbaudīto to sakritību.
    */
    println("TASK 5: Describe a number's sign using match")
    
    for (n <- List(-3, 0, 4)) {
        println(
            s"Number $n: using if/else = ${describeSign(n)}, " +
            s"using match = ${describeSignMatch(n)}"
        )
    }

    println()

    /** 6. uzdevums
     * 
     * Izsauc asinhrono funkciju un ar foreach izdrukā 
     * iegūto rezultātu: The answer is 6.
     * 
     * Programmas sākumpunktā gaida lietotāja ievadi,
     * lai programma nebeigtos pirms rezultāta izdrukāšanas.
     */
    println("TASK 6: Calculate a sum and build a message asynchronously")
    
    answerAsync().foreach { answer =>
        println(s"Asynchronous result: $answer")
        println()
    }

    println("Waiting for the asynchronous result. " +
      "Press ENTER after it appears to exit.")
    scala.io.StdIn.readLine()
}