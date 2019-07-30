// Copyright (c) Microsoft Corporation. All Rights Reserved. See License.txt in the project root for license information.

/// Coordinating compiler operations - configuration, loading initial context, reporting errors etc.
module internal FSharp.Compiler.Features

open System

//------------------------------------------------------------------------------------------------------------------
// Language version command line switch
//------------------------------------------------------------------------------------------------------------------
// Add your features to this List - in code use languageVersion.SupportsFeature(LanguageFeatures.yourFeature) 
// a return value of false means your feature is not supported by the user's language selection
// All new language features added from now on must be protected by this.
// Note:
//   *  The fslang design process will require a decision about feature name and whether it is required.
//   *  When a feature is assigned a release language, we will scrub the code of feature references and apply
//      the Release Language version.

/// LanguageFeature enumeration
[<RequireQualifiedAccess>]
type LanguageFeature =
    | PreviewVersion = 0
    | LanguageVersion46 = 1
    | LanguageVersion47 = 2
    | SingleUnderscorePattern = 3
    | WildCardInForLoop = 4
    | RelaxWhitespace = 5
    | NameOf = 6
    | DefaultInterfaceMethodConsumption = 7
    | ImplicitYield = 8
    | OpenStaticClasses = 9


/// LanguageVersion management
type LanguageVersion (specifiedVersion) =

    // When we increment language versions here preview is higher than current RTM version
    static let languageVersion46 = 4.6m
    static let languageVersion47 = 4.7m
    static let previewVersion = 9999m                   // Language version when preview specified
    static let defaultVersion = languageVersion47       // Language version when default specified
    static let latestVersion = defaultVersion           // Language version when latest specified
    static let latestMajorVersion = languageVersion46   // Language version when latestmajor specified

    static let validOptions = [| "preview"; "default"; "latest"; "latestmajor" |]
    static let languageVersions = set [| languageVersion46; languageVersion47 |]

    static let features = dict [|
        // Add new LanguageVersions here ...
        LanguageFeature.LanguageVersion46, languageVersion46
        LanguageFeature.LanguageVersion47, languageVersion47
        LanguageFeature.PreviewVersion, previewVersion
        LanguageFeature.SingleUnderscorePattern, languageVersion47
        LanguageFeature.WildCardInForLoop, languageVersion47
        LanguageFeature.RelaxWhitespace, languageVersion47
        LanguageFeature.NameOf, languageVersion47
        LanguageFeature.ImplicitYield, languageVersion47
        LanguageFeature.DefaultInterfaceMethodConsumption, languageVersion47
        LanguageFeature.OpenStaticClasses, languageVersion47
        |]

    let specified =
        match specifiedVersion with
        | "?" -> 0m
        | "preview" -> previewVersion
        | "default" -> latestVersion
        | "latest" -> latestVersion
        | "latestmajor" -> latestMajorVersion
        | _ ->
            match Decimal.TryParse(specifiedVersion) with
            | true, v -> v
            | _ -> 0m

    /// Check if this feature is supported by the selected langversion
    member __.SupportsFeature featureId =
        match features.TryGetValue featureId with
        | true, v -> v <= specified
        | false, _ -> false

    /// Does the languageVersion support this version string
    member __.ContainsVersion version =
        match version with
        | "?" | "preview" | "default" | "latest" | "latestmajor" -> true
        | _ -> 
            match Decimal.TryParse(specifiedVersion) with
            | true, v -> languageVersions.Contains v
            | _ -> false

    /// Get a list of valid strings for help text
    member __.ValidOptions = validOptions

    /// Get a list of valid versions for help text
    member __.ValidVersions = [|
        for v in languageVersions |> Seq.sort do
            let label = if v = defaultVersion then " (Default)" else ""
            yield sprintf "%M%s" v label
            |]

    /// Get a string name for the given feature. Returns an empty string if feature is invalid.
    member __.GetFeatureString featureId =
        match featureId with
        | LanguageFeature.PreviewVersion -> "preview"
        | LanguageFeature.LanguageVersion46 -> "4.6"
        | LanguageFeature.LanguageVersion47 -> "4.7"
        | LanguageFeature.SingleUnderscorePattern -> "single underscore pattern"
        | LanguageFeature.WildCardInForLoop -> "wild card in for loop"
        | LanguageFeature.RelaxWhitespace -> "whitespace relexation"
        | LanguageFeature.NameOf -> "nameof"
        | LanguageFeature.DefaultInterfaceMethodConsumption -> "default interface method consumption"
        | LanguageFeature.ImplicitYield -> "implicit yield"
        | LanguageFeature.OpenStaticClasses -> "open static classes"
        | _ -> String.Empty

    /// Get a version string associated with the given feature. Returns an empty string if feature is invalid.
    member __.GetFeatureVersionString featureId =
        match features.TryGetValue featureId with
        | true, v when v = previewVersion -> "'preview'"
        | true, v -> string v
        | _ -> String.Empty

    /// Current version string.  @@@@@@@@@@@@
    member val CurrentVersionString = string specified

    /// Get the specified LanguageVersion
    member __.SpecifiedVerson = specified
