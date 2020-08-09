module Server.ProfessionalSyndics.ProfessionalSyndicCache

open System
open System.Collections.Generic
open Server.Blueprint.Behavior.ProfessionalSyndics

type ProfessionalSyndicCache (conn: string) =
    member private _.Cache = new Dictionary<Guid, Guid list>()

    interface IProfessionalSyndicCache with
        member me.ClearCache () =
            me.Cache.Clear()
        member me.GetBuildingIdsForProfessionalSyndic professionalSyndicId =
            if not (me.Cache.ContainsKey (professionalSyndicId))
            then
                me.Cache.[professionalSyndicId]
            else
                let professionalSyndic =
                    Query.getProfessionalSyndic conn professionalSyndicId
                    |> Async.RunSynchronously
                match professionalSyndic with
                | Some professionalSyndic ->
                    me.Cache.Add(professionalSyndicId, professionalSyndic.BuildingIds)
                    professionalSyndic.BuildingIds
                | None ->
                    []