module Server.MailProvider

open MimeKit
open AppSettings
open MailKit.Security
open System.Net.Security
open System.Security.Cryptography.X509Certificates
open Serilog

let private validateServerCertificate (sender: obj) (certificate: X509Certificate) (chain: X509Chain) (sslPolicyErrors: SslPolicyErrors) =
        if sslPolicyErrors = SslPolicyErrors.None        
        then true
        else
            // Note: MailKit will always pass the host name string as the `sender` argument.
            let host = string sender;

            if (sslPolicyErrors &&& SslPolicyErrors.RemoteCertificateNotAvailable) <> SslPolicyErrors.None
            then
                Log.Logger.Error(sprintf "The SSL certificate was not available for %s" host)

            if (sslPolicyErrors &&& SslPolicyErrors.RemoteCertificateNameMismatch) <> SslPolicyErrors.None
            then
                // This means that the server's SSL certificate did not match the host name that we are trying to connect to.
                let certificate2 = certificate :?> X509Certificate2
                let cn = if certificate2 <> null then (certificate2.GetNameInfo (X509NameType.SimpleName, false)) else (certificate.Subject)

                Log.Logger.Error(sprintf "The Common Name for the SSL certificate did not match %s. Instead, it was %s." host cn)

            // The only other errors left are chain errors.
            Log.Logger.Error(sprintf "The SSL certificate for the server could not be validated for the following reasons:")

            // The first element's certificate will be the server's SSL certificate (and will match the `certificate` argument)
            // while the last element in the chain will typically either be the Root Certificate Authority's certificate -or- it
            // will be a non-authoritative self-signed certificate that the server admin created. 
            for element in chain.ChainElements do
                // Each element in the chain will have its own status list. If the status list is empty, it means that the
                // certificate itself did not contain any errors.
                if element.ChainElementStatus.Length = 0
                then ()
                else
                    Log.Logger.Error(sprintf "\u2022 %s" element.Certificate.Subject)
                    for error in element.ChainElementStatus do
                        // `error.StatusInformation` contains a human-readable error string while `error.Status` is the corresponding enum value.
                        Log.Logger.Error(sprintf "\t\u2022 %s" error.StatusInformation)
            false

let private sendMail (settings: MailSettings) (message: MimeMessage) = async {
    use smtp = new MailKit.Net.Smtp.SmtpClient()
    smtp.ServerCertificateValidationCallback <- (fun sender certificate chain errors -> 
        validateServerCertificate sender certificate chain errors)
    
    try 
        do! smtp.ConnectAsync(host = settings.SmtpServer, port = settings.Port, options = if settings.UseTLS then SecureSocketOptions.StartTls else SecureSocketOptions.Auto) |> Async.AwaitTask
        do! smtp.AuthenticateAsync(settings.ServiceUsername, settings.ServicePassword) |> Async.AwaitTask
        do! smtp.SendAsync(message) |> Async.AwaitTask
        do! smtp.DisconnectAsync(true) |> Async.AwaitTask
        return Ok ()
    with error ->
        Log.Logger.Error(error, "An error occured while trying to send an e-mail")
        return Error ()
}

let sendSupportEmail (settings: MailSettings) (recipients: InternetAddress list) (subject: string, body: MimeEntity) = async {
    let message = new MimeMessage ();
    message.From.Add (new MailboxAddress ("Support", "support@syndicusassistent.be"));
    message.To.AddRange(seq { yield! recipients |> List.toSeq })
    message.Subject <- subject;
    message.Body <- body

    return! sendMail settings message
}