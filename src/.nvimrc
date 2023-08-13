
lua <<EOF
require'lspconfig'.purescriptls.setup{
  settings = {
    purescript = {
      censorWarnings = {"ShadowedName","ImplicitQualifiedImport","HidingImport","WildcardInferredType"}
    }
  }
}
EOF
