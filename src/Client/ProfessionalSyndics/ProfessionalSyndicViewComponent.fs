module Client.ProfessionalSyndics.ProfessionalSyndicViewComponent
open Fable.React
open Shared.Read
open Client.Organizations

let view (professionalSyndic: ProfessionalSyndic) =
    div [] [
        OrganizationViewComponent.render {| Organization = professionalSyndic.Organization |}
    ]

let render =
    FunctionComponent.Of ((fun (props: {| ProfessionalSyndic: ProfessionalSyndic |}) -> view props.ProfessionalSyndic), memoizeWith = memoEqualsButFunctions)