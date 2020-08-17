module Server.AppSettings

[<CLIMutable>]
type DatabaseSettings = {
    Connection: string
}

[<CLIMutable>]
type GoogleSettings = {
    CloudApiKey: string
    RecaptchaClientKey: string
    RecaptchaServerKey: string
}

[<CLIMutable>]
type SysAdmin = {
    EmailAddress: string
    DisplayName: string
    PreferredLanguageCode: string
    Password: string
}

[<CLIMutable>]
type AuthenticationSettings = {
    PasswordPepper: string
    ChangePasswordSigningKey: string
    UsernamePasswordSigningKey: string
    TwoFacPassword: string
    TwoFacPepper: string
    MaxNbTwoFacAttempts: int
    SysAdmins: SysAdmin[]
}

[<CLIMutable>]
type AppSettings = {
    Database: DatabaseSettings
    Google: GoogleSettings
    Authentication: AuthenticationSettings
}