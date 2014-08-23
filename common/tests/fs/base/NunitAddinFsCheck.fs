namespace Base

open FsCheck.NUnit.Addin
open NUnit.Core.Extensibility

[<NUnitAddin(Description = "FsCheck addin")>]
type NunitAddinFsCheck () =
    interface IAddin with
       override x.Install host =
           let tcBuilder = new FsCheckTestCaseBuider()
           host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
           true
