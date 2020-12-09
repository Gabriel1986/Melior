module Server.Encryption
    //Based on https://tomrucki.com/posts/aes-encryption-in-csharp/

    open System
    open System.Security.Cryptography
    open System.Text

    let [<Literal>] AesBlockByteSize = 16 // = 128 / 8
    let [<Literal>] PasswordSaltByteSize = 16 // = 128 / 8
    let [<Literal>] PasswordByteSize = 32 // = 256 / 8
    let [<Literal>] PasswordIterationCount = 100000 //lastpass standard
    let [<Literal>] SignatureByteSize = 32 // = 256 / 8

    let MinimumEncryptedMessageByteSize =
        PasswordSaltByteSize + // auth salt
        PasswordSaltByteSize + // key salt
        AesBlockByteSize + // IV
        AesBlockByteSize + // cipher text min length
        SignatureByteSize // signature tag

    let private rng = RandomNumberGenerator.Create()
    let private createAes () = Aes.Create(Mode = CipherMode.CBC, Padding = PaddingMode.PKCS7)
    
    let private generateCipherText (toEncrypt: string, key: byte[], iv: byte[]) =
        use aes = createAes()
        use encryptor = aes.CreateEncryptor(key, iv) 
        (
            let plainText = Encoding.UTF8.GetBytes(toEncrypt)
            encryptor.TransformFinalBlock(plainText, 0, plainText.Length)
        )

    let private generateRandomByteArray(numberOfBytes: int) =
        let randomBytes = Array.zeroCreate<byte> numberOfBytes
        rng.GetBytes(randomBytes)
        randomBytes

    let generateRandomString(numberOfCharacters: int) =
        generateRandomByteArray (numberOfCharacters)
        |> Encoding.UTF8.GetString

    let private getKey(password: string, passwordSalt: byte[]) =
        let keyBytes = Encoding.UTF8.GetBytes(password);
        use derivator = new Rfc2898DeriveBytes(keyBytes, passwordSalt, PasswordIterationCount, HashAlgorithmName.SHA256)
        derivator.GetBytes(PasswordByteSize);

    let encryptString(toEncrypt: string, password: string) =
        //encrypt
        let keySalt = generateRandomByteArray(PasswordSaltByteSize);
        let key = getKey(password, keySalt);

        let iv = generateRandomByteArray(AesBlockByteSize);
        let cipherText = generateCipherText (toEncrypt, key, iv)

        let authKeySalt = generateRandomByteArray(PasswordSaltByteSize);
        let authKey = getKey(password, authKeySalt);

        let result = Array.concat [ authKeySalt; keySalt; iv; cipherText ]

        //create signature
        use hmac = new HMACSHA256(authKey)
        (
            hmac.ComputeHash(result, 0, result.Length)
            |> Array.append result
        )

    let decryptToString(encryptedData: byte[], password: string) =
        if (encryptedData = null || encryptedData.Length < MinimumEncryptedMessageByteSize)
        then raise (new ArgumentException("Invalid length of encrypted data", nameof encryptedData))

        let authKeySalt = encryptedData.AsSpan(0, PasswordSaltByteSize).ToArray();
        let keySalt = encryptedData.AsSpan(PasswordSaltByteSize, PasswordSaltByteSize).ToArray();
        let iv = encryptedData.AsSpan(2 * PasswordSaltByteSize, AesBlockByteSize).ToArray();
        let signatureTag = encryptedData.AsSpan(encryptedData.Length - SignatureByteSize, SignatureByteSize).ToArray();

        let cipherTextIndex = authKeySalt.Length + keySalt.Length + iv.Length;
        let cipherTextLength = encryptedData.Length - cipherTextIndex - signatureTag.Length;

        let authKey = getKey(password, authKeySalt);
        let key = getKey(password, keySalt);

        // verify signature
        use hmac = new HMACSHA256(authKey)
        (
            let payloadToSignLength = encryptedData.Length - SignatureByteSize;
            let signatureTagExpected = hmac.ComputeHash(encryptedData, 0, payloadToSignLength);

            // constant time checking to prevent timing attacks
            let invalidSignatures = 
                [
                    yield! signatureTag |> Array.mapi (fun index signature -> if signature = signatureTagExpected.[index] then None else Some false)
                ]
                |> List.choose id
            
            if not invalidSignatures.IsEmpty
            then raise (new CryptographicException("Invalid signature"))
        )

        // decrypt
        use aes = createAes()
        (
            use encryptor = aes.CreateDecryptor(key, iv)
            (
                let decryptedBytes = encryptor.TransformFinalBlock(encryptedData, cipherTextIndex, cipherTextLength);
                Encoding.UTF8.GetString(decryptedBytes);
            )
        )

    let verifyPassword (pepper: string) (password: string, encryptedHashedPassword: byte[]) =
        let hashedPassword = decryptToString (encryptedHashedPassword, pepper)
        BCrypt.Net.BCrypt.EnhancedVerify (password, hashedPassword)

    let hashPassword (pepper: string) (password: string): byte[] =
        let hashedPassword = BCrypt.Net.BCrypt.EnhancedHashPassword (password)
        encryptString (hashedPassword, pepper)