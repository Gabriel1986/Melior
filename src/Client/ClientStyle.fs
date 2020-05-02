module Client.ClientStyle

open Zanaptak.TypedCssClasses

type Bootstrap = CssClasses<"public/styles/bootstrap.min.css", Naming.CamelCase, resolutionFolder=__SOURCE_DIRECTORY__>
type FontAwesome = CssClasses<"public/styles/fontawesome.all.min.css", Naming.CamelCase, resolutionFolder=__SOURCE_DIRECTORY__>
