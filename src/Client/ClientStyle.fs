module Client.ClientStyle

open Zanaptak.TypedCssClasses

type MeliorStyle = CssClasses<"public/styles/bootstrap.min.css", Naming.CamelCase, resolutionFolder=__SOURCE_DIRECTORY__>


