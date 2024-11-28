# Função para buscar os dados da API REDCap
fetch_redcap_data <- function(api_url, api_key) {
    response <- POST(api_url, body = list(
        token = api_key,
        content = "record",
        format = "json",
        type = "flat"
    ), encode = "form")
    
    if (response$status_code == 200) {
        data <- fromJSON(content(response, "text", encoding = "UTF-8"))
        return(data)
    } else {
        stop("Erro ao conectar na API REDCap.")
    }
}