//
// F# program to analyze Divvy daily ride data.
//
// << Raj Patel >>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #04
//

#light

module project04

open System.Net

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [176;74;1252;21;595;1986;1]; ... ]
//
// station id (from), station id (to), bike id, starting hour (0..23), trip duration (secs), birth
// year (0=>not specified), and gender (0=>not specified, 
// 1=>identifies as male, 2=>identifies as female).
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints


let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

let rec totalgender L genNum =  //to find the total number of any gender
    match L with
    |[] ->0
    |[[]] ->0
    |hd::tl when hd.Item(6)=genNum ->1+totalgender tl genNum
    |hd::tl ->0+totalgender tl genNum
   
let percentgender total  totalgen =  //to find the percent of the any gender
    match total with
    | 0 -> 0.0
    |_ ->(float(totalgen)/float(total))*100.0


let rec age L =      //getting the list of valid age
    match L with
    | []->[]
    |[[]] ->[]
    |hd::tl when hd.Item(5)=0 ->age tl
    |hd::tl ->(System.DateTime.Now.Year - hd.Item(5)):: age tl
 

let rec sum L =    //function to add up
    match L with
    | [] ->0
    |hd::tl ->hd+sum tl

let avgcal sum total =       //calculating avg age
    match total with
    | 0 ->0.0
    |_ ->(float(sum)/float(total)*100.0)

let rec duration t1 t2 L =
    match L with
    |[] ->0
    |[[]] ->0
    |hd::tl when ((float (hd.Item(4))/60.0)>t1) && ((float(hd.Item(4))/60.0)<=t2) ->1+ duration t1 t2 tl
    |hd::tl ->duration t1 t2 tl

let rec printstars n =
    match n with
    |0 ->()
    |1 ->printf"*"
    |_ ->printf "*"
         printstars (n-1)   

let rec starthour t1 L=   //calculating the numbers
    match L with 
    | [] ->0
    |[[]] -> 0
    |hd::tl when hd.Item(3)=t1 ->1+ starthour t1 tl
    |hd::tl -> starthour t1 tl 
    
let rec printstart  n L =      //histogram
    match n with
    |24  -> ()
    |n ->printf " %A: " n
         printstars((starthour n L)/10)
         printfn "%A"  (starthour n L)   
         printstart (n+1) L
                            
    
let AgeAv sum total =
    match total with
    |0 ->0.0
    |_ ->(float(sum)/float(total))
    

[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  // 
  
  
  printf "filename> "
  let filename = System.Console.ReadLine()
  //let filename="divvy2.csv"
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents
  let malepercent= (percentgender(ridedata.Length)(totalgender ridedata 1))
  let femalepercent=(percentgender(ridedata.Length)(totalgender ridedata 2))
  let ageAvg =(AgeAv(sum(age ridedata)) ((age ridedata).Length))

  //printfn "%A" ridedata
  let N = List.length ridedata
  
  
  printfn ""
  printfn "# of riders: %A" N
  printfn ""
  printfn "%% of riders identifying as male: %A (%A%%)" (totalgender ridedata 1) malepercent 
  printfn "%% of riders identifying as female: %A (%A%%)" (totalgender ridedata 2) femalepercent
  printfn ""
  printfn "Average age: %A" ageAvg
  printfn ""
  printfn "** Ride Durations:"
  printfn " 0..30 mins: %A (%A%%)" (duration 0.0 30.0 ridedata) (avgcal(duration 0.0 30.0 ridedata)(ridedata.Length))
  printfn " 30..60 mins: %A (%A%%)" (duration 30.0 60.0 ridedata) (avgcal(duration 30.0 60.0 ridedata)(ridedata.Length))
  printfn " 60..120 mins: %A (%A%%)" (duration 60.0 120.0 ridedata) (avgcal(duration 60.0 120.0 ridedata)(ridedata.Length))
  printfn " > 2 hours: %A (%A%%)" (duration 120.0 1200.0 ridedata) (avgcal(duration 120.0 1200.0 ridedata)(ridedata.Length))
  printfn ""
  printfn "** Ride Start Time Histogram:"
  printstart 0 ridedata 
  0 