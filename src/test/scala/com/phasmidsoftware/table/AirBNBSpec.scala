package com.phasmidsoftware.table

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.resource
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class AirBNBSpec extends AnyFlatSpec with Matchers {

  behavior of "AirBNB table"

  it should "ingest multiline entry 1" in {
    val text =
      """"name"
        |"Robin
        |Hillyard"""".stripMargin
    val sy = IO(Source.fromString(text))

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setMultiline(true)

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

    matchIO(wsty, Timeout(Span(4000, Seconds))) {
      case t@HeadedTable(_, _) =>
        t.foreach(r => println("**********" + r))
        val analysis = Analysis(t)
        println(s"AirBNB: $analysis")
        analysis.rows shouldBe 1
        succeed
    }
  }

  // TODO do we really care if this stupid entry can be parsed??
  ignore should "ingest multiline entry 2" in {
    val text =
      """"name","summary","space","description","neighborhood_overview","notes","transit","access","interaction","house_rules","id","Year","Month","listing_url","picture_url","property_type","room_type","accommodates","bathrooms","bedrooms","beds","bed_type","amenities","square_feet","price","weekly_price","monthly_price","security_deposit","cleaning_fee","guests_included","extra_people","minimum_nights","maximum_nights","calendar_updated","availability_30","availability_60","availability_90","availability_365","calendar_last_scraped","requires_license","license","instant_bookable","cancellation_policy","require_guest_profile_picture","require_guest_phone_verification","is_business_travel_ready","host_id","host_url","host_name","host_since","host_location","host_about","host_response_time","host_response_rate","host_acceptance_rate","host_is_superhost","host_thumbnail_url","host_picture_url","host_total_listings_count","host_verifications","host_has_profile_pic","host_identity_verified","calculated_host_listings_count","calculated_host_listings_count_entire_homes","calculated_host_listings_count_private_rooms","calculated_host_listings_count_shared_rooms","number_of_reviews","number_of_reviews_ltm","reviews_per_month","first_review","last_review","review_scores_rating","review_scores_accuracy","review_scores_cleanliness","review_scores_checkin","review_scores_communication","review_scores_location","review_scores_value","latitude","longitude","street","is_location_exact","airbnb_region","Blk_ID_10","CT_ID_10","TOWN","COUNTY"
        |Cozy, simple & practical private room IN Boston!","This is maybe the most convenient place, affordable and very close to public transportation in 15 min by bus to Boston Downtown! Amazing neighborhood, close to restaurants, shops, and big malls. The famous casino of Boston is like 2 min walk from the house. It's a basic room with a twin bed. So make sure you bring any extra things you might need. I hope you will enjoy your stay, and please message me for any questions. There might be potential noise from construction nearby.","You will love this place and its location","This is maybe the most convenient place, affordable and very close to public transportation in 15 min by bus to Boston Downtown! Amazing neighborhood, close to restaurants, shops, and big malls. The famous casino of Boston is like 2 min walk from the house. It's a basic room with a twin bed. So make sure you bring any extra things you might need. I hope you will enjoy your stay, and please message me for any questions. There might be potential noise from construction nearby. You will love this place and its location All common areas I will pass by to say hello when i am around Assembly row is a 10 min walk where you will find all types of coffee shops, restaurants and movie theaters. Around the house there is nothing much other then constructions. Uber or public transportation Bedroom doors have no locks, but it is very safe, and parking is available on a first come first served basis. USA Taxes request will be sent through airbnb after the booking is over.","Assembly row is a 10 min walk where you will find all types of coffee shops, restaurants and movie theaters. Around the house there is nothing much other then constructions.","Bedroom doors have no locks, but it is very safe, and parking is available on a first come first served basis. USA Taxes request will be sent through airbnb after the booking is over.","Uber or public transportation","All common areas","I will pass by to say hello when i am around","We do have a flexible check in but this means that guests can leave their luggages in the common area while waiting for the previous guest to check out (usually at 9 am) and the cleaning team to clean the room. We want to make it so smooth for the guests, and we would appreciate your cooperation on informing us about your schedule, and we will do our duty and informing you when the room is ready which should be usually between (12 pm - 6 pm)",31460036,2019,"January","https://www.airbnb.com/rooms/31460036","https://a0.muscache.com/im/pictures/ca063a50-6df2-4014-9772-201ac4df878d.jpg?aki_policy=large","APT","PR",2,2,1,1,"RB","{TV,Internet,Wifi,Kitchen,""Free parking on premises"",Heating,Washer,Dryer,""Smoke detector"",""Carbon monoxide detector"",Essentials,Shampoo,Hangers,""Laptop friendly workspace"",""Self check-in"",Lockbox,""Hot water"",""Bed linens"",""Extra pillows and blankets"",Microwave,""Coffee maker"",Refrigerator,""Dishes and silverware"",""Cooking basics"",Oven,Stove,""Luggage dropoff allowed"",""Long term stays allowed""}",NA,"$42.00 ","","","","$65.00 ",1,"$35.00 ",1,1125,"today",14,44,74,349,"1/17/19",0,"",1,"STR_G",0,0,0,25149641,"https://www.airbnb.com/users/show/25149641","Kiki","12/23/14","Boston, Massachusetts, United States","We are brothers fortunate to work/study full time between great cities such as Miami, Boston, New York, Providence, Berkshires & Beirut. Airbnb is a side gig. Some places are very basic, others are of great value. Our places are for people traveling on a budget. We have great cohosts that will take care of you in case we are not available. We love to travel, and connect with people. We have a passion for physics, engineering and rare diseases. This account is professionally managed.
        |
        |Life Moto:
        |“We are all travelers in this world. From the sweet grass to the packing house, birth till death, we travel between the eternities.”","within an hour","100%",NA,"0","https://a0.muscache.com/im/pictures/user/3bbc5f44-5fab-42c9-8cbf-88cbd8c28340.jpg?aki_policy=profile_small","https://a0.muscache.com/im/pictures/user/3bbc5f44-5fab-42c9-8cbf-88cbd8c28340.jpg?aki_policy=profile_x_medium",60,"['email', 'phone', 'reviews', 'jumio', 'selfie', 'government_id', 'identity_manual', 'work_email']","1","1",24,1,21,2,0,0,0,"","",NA,NA,NA,NA,NA,NA,NA,42.39408839,-71.06429301,"Everett, MA, United States",0,"Boston",2.50173e+14,25017342400,"Everett","Middlesex County"
        |
        |""".stripMargin
    val sy = IO(Source.fromString(text))

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setMultiline(true)

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

    matchIO(wsty, Timeout(Span(4000, Seconds))) {
      case t@HeadedTable(_, _) =>
        t.foreach(r => println("**********" + r))
        val analysis = Analysis(t)
        println(s"AirBNB: $analysis")
        analysis.rows shouldBe 1
        succeed
    }
  }

  it should "be ingested properly" in {
    val airBNBFile = "/airbnb2.csv"

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- resource[AirBNBSpec](airBNBFile)) yield Source.fromURL(u))

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(2)).setMultiline(true)

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

    matchIO(wsty, Timeout(Span(4, Seconds))) {
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"AirBNB: $analysis")
        analysis.rows shouldBe 127 +- 32
        r take 100 foreach println
        succeed
    }
  }
}
