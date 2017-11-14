// Include Fake library
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.AssemblyInfoFile
open System
open System.IO

// Properties
let prodName = "MeanDiff"
let prodDesc = "MeanDiff: BBIR testing tool"
let licenseStr = "Copyright (c) KAIST, since 2017"
let releaseNotes =
  ReadFile "ReleaseNotes.md"
  |> ReleaseNotesHelper.parseReleaseNotes

let buildDir = "./build/"
let buildMode = getBuildParamOrDefault "mode" "Debug"

let createAsmInfo path title product guid =
  CreateFSharpAssemblyInfo path
    [
      Attribute.Title title
      Attribute.Product product
      Attribute.Description prodDesc
      Attribute.Version releaseNotes.AssemblyVersion
      Attribute.FileVersion releaseNotes.AssemblyVersion
      Attribute.Guid guid
      Attribute.Copyright licenseStr
    ]

let buildProj projName guid =
  let projDir = projName
  let projFile = projName + ".fsproj"
  let platform = if Environment.Is64BitOperatingSystem then "x64" else "x86"
  let isMSBuild = (Path.GetFileName (msBuildExe)).ToLower().StartsWith("msbuild")
  let setParams defaults =
    {
      defaults with
        Verbosity = Some Normal
        Targets = ["Build"]
        Properties =
          [
            "Platform", platform
            "Optimize", if buildMode = "Debug" then "False" else "True"
            "DebugSymbols", "True"
            "Configuration", buildMode
            "DefineConstants", getBuildParamOrDefault "define" ""
            "AllowedReferenceRelatedFileExtensions", "none"
            "TargetFrameworkVersion", getBuildParamOrDefault "framework" "v4.5"
          ]
        ToolsVersion = if isMSBuild then None else Some "12.0"
    }

  createAsmInfo (projDir @@ "AssemblyInfo.fs") projName prodName guid
  build setParams (projDir @@ projFile) |> DoNothing

// Targets
Target "Clean" (fun _ ->
  CleanDir buildDir
  Directory.Delete buildDir
  CleanDir "./MeanDiff/obj/"
  Directory.Delete "./MeanDiff/obj/"
  File.Delete "./MeanDiff/AssemblyInfo.fs"
)

Target "Package" (fun _ ->
  RestorePackages ()
)

Target "MeanDiff" (fun _ ->
  buildProj "MeanDiff" "346faf23-b624-4861-ae72-0efb43ad4b68"
)

Target "All" DoNothing

// Dependencies
"Package" ==> "All"
"MeanDiff" ==> "All"

// Start build
RunTargetOrDefault "All"
