open Google.Apis.Books.v1

let getService () =
    new BooksService(BooksService.Initializer())

type Rating =
    { Average: float option
      Count: int }

type ImageLinks =
    { SmallThumbnail: string
      Thumbnail: string
      Small: string
      Medium: string
      Large: string
      ExtraLarge: string }

type Volume =
    { Title: string
      Authors: string list
      Publisher: string
      PublishedDate: string
      Description: string
      PageCount: int option
      Categories: string list
      Rating: Rating
      ContentVersion: string
      ImageLinks: ImageLinks option
      Language: string }

module Seq =
    let safe (seq: seq<_>) =
        if seq = null then
            Seq.empty
        else
            seq

let searchVolumes q (service: BooksService) = async {
    let request = service.Volumes.List(q)

    let! response =
        request.ExecuteAsync()
        |> Async.AwaitTask

    let toData (volume: Data.Volume) =
        let rating =
            { Average = Option.ofNullable volume.VolumeInfo.AverageRating
              Count =
                Option.ofNullable volume.VolumeInfo.RatingsCount
                |> Option.defaultValue 0 }
        let imageLinks =
            volume.VolumeInfo.ImageLinks
            |> Option.ofObj
            |> Option.map (fun imageLinks ->
                { SmallThumbnail = imageLinks.SmallThumbnail
                  Thumbnail = imageLinks.Thumbnail
                  Small = imageLinks.Small
                  Medium = imageLinks.Medium
                  Large = imageLinks.Large
                  ExtraLarge = imageLinks.ExtraLarge })
        { Title = volume.VolumeInfo.Title
          Authors = List.ofSeq (Seq.safe volume.VolumeInfo.Authors)
          Publisher = volume.VolumeInfo.Publisher
          PublishedDate = volume.VolumeInfo.PublishedDate
          Description = volume.VolumeInfo.Description
          PageCount = Option.ofNullable volume.VolumeInfo.PageCount
          Categories = List.ofSeq (Seq.safe volume.VolumeInfo.Categories)
          Rating = rating
          ContentVersion = volume.VolumeInfo.ContentVersion
          ImageLinks = imageLinks
          Language = volume.VolumeInfo.Language }

    return
        response.Items
        |> Seq.safe
        |> Seq.map toData
        |> Seq.toList
}

open Argu

type Arguments =
    | [<AltCommandLineAttribute("--title", "-t")>] InTitle of string
    | [<AltCommandLineAttribute("--author", "-a")>] InAuthor of string
    | [<AltCommandLineAttribute("--publisher", "-p")>] InPublisher of string
    | [<AltCommandLineAttribute "-s">] Subject of string
    | [<AltCommandLineAttribute "-i">] Isbn of string
    | [<AltCommandLineAttribute "-l">] Lccn of string
    | [<AltCommandLineAttribute "-o">] Oclc of string
    | Raw
    interface IArgParserTemplate with
        member self.Usage =
            match self with
            | InTitle _ -> "Search in the title of the book"
            | InAuthor _ -> "Search in the author of the book"
            | InPublisher _ -> "Search in the publisher of the book"
            | Subject _ -> "Search in the subject of the book"
            | Isbn _ -> "Search in the ISBN code of the book"
            | Lccn _ -> "Search in the LCCN code of the book"
            | Oclc _ -> "Search in the OCLC code of the book"
            | Raw -> "Output raw information"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>(errorHandler = ProcessExiter())
    let result = parser.ParseCommandLine argv

    let inTitle = result.TryGetResult <@ InTitle @>
    let inAuthor = result.TryGetResult <@ InAuthor @>
    let inPublisher = result.TryGetResult <@ InPublisher @>
    let subject = result.TryGetResult <@ Subject @>
    let isbn = result.TryGetResult <@ Isbn @>
    let lccn = result.TryGetResult <@ Lccn @>
    let oclc = result.TryGetResult <@ Oclc @>

    let q =
        [ inTitle |> Option.map (fun title -> "intitle:" + title) |> Option.toList
          inAuthor |> Option.map (fun author -> "inauthor:" + author) |> Option.toList
          inPublisher |> Option.map (fun publisher -> "inpublisher:" + publisher) |> Option.toList
          subject |> Option.map (fun subject -> "subject:" + subject) |> Option.toList
          isbn |> Option.map (fun isbn -> "isbn:" + isbn) |> Option.toList
          lccn |> Option.map (fun lccn -> "lccn:" + lccn) |> Option.toList
          oclc |> Option.map (fun oclc -> "oclc:" + oclc) |> Option.toList ]
        |> List.concat
        |> String.concat "+"

    if q = "" then
        parser.PrintUsage("Please provide a title, author, publisher, subject, ISBN code, LCCN code or OCLC code")
        |> printfn "%s"


    else
        use service = getService ()

        let raw = result.Contains <@ Raw @>
        let print volume =
            if raw then
                printfn "%A" volume
            else
                printfn
                    "%s (%s)"
                    volume.Title
                    (String.concat ", " volume.Authors)

        searchVolumes q service
        |> Async.RunSynchronously
        |> List.distinctBy (fun volume -> volume.Title, volume.Authors) // filter out duplicates
        |> List.iter print

    0
