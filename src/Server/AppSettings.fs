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
type MailSettings = {
    From: string
    FromDisplayName: string
    ServiceUsername: string
    ServicePassword: string
    SmtpServer: string
    UseTLS: bool
    Port: int
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
    BaseUrl: string
    Database: DatabaseSettings
    Google: GoogleSettings
    Mail: MailSettings
    Authentication: AuthenticationSettings
}