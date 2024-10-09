


# Função para criar as pastas e subpastas
criar_pastas <- function(root_dir = "meu_projeto") {
  
  # Lista de pastas e subpastas a serem criadas
  pastas <- c(
    root_dir,
    paste0(root_dir, "/data"),        # Dados brutos ou pré-processados
    paste0(root_dir, "/data_raw"),    # Dados brutos que não devem ser alterados
    paste0(root_dir, "/utilities"), # Funções e scripts que podem ser úteis
    paste0(root_dir, "/codes"),       # Scripts de análise (reprodutíveis)
    paste0(root_dir, "/results"),     # Resultados (modelos, gráficos, tabelas)
    paste0(root_dir, "/outputs"),     # Relatórios finais, figuras, exportações
    paste0(root_dir, "/docs"),        # Manuscrito, documentação, relatórios (e.g., RMarkdown, LaTeX, Word)
    paste0(root_dir, "/renv"),        # Ambiente R (dependências de pacotes)
    paste0(root_dir, "/tests"),        # Testes unitários (opcional)
    paste0(root_dir, "/images")
    )
  
  
  # Criar as pastas se elas não existirem
  for (pasta in pastas) {
    if (!dir.exists(pasta)) {
      dir.create(pasta, recursive = TRUE)
      message("Pasta criada: ", pasta)
    } else {
      message("Pasta já existe: ", pasta)
    }
  }
  
  # Arquivos adicionais
  file.create(paste0(root_dir, "/README.md"))
  file.create(paste0(root_dir, "/.gitignore"))
  message("Arquivos README.md e .gitignore criados.")
}


# Execute a função para criar as pastas
# criar_pastas("C:/Users/LENOVO/OneDrive/POS-DOC/TPC_organizado")
