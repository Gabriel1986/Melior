module Server.AppSettings

[<CLIMutable>]
type DatabaseSettings = {
    ConnectionString: string
}

[<CLIMutable>]
type GoogleSettings = {
    CloudApiKey: string
    RecaptchaClientKey: string
    RecaptchaServerKey: string
}

[<CLIMutable>]
type AuthenticationSettings = {
    PasswordPepper: string
    ChangePasswordSigningKey: string
    UsernamePasswordSigningKey: string
    TwoFacPassword: string
    TwoFacPepper: string
    MaxNbTwoFacAttempts: int
}

[<CLIMutable>]
type AppSettings = {
    Database: DatabaseSettings
    Google: GoogleSettings
    Authentication: AuthenticationSettings
}