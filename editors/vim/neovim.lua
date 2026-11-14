-- sift --- a Neovim setup
--
-- SPDX-License-Identifier: MIT
-- Copyright © Josep Bigorra <jjbigorra@gmail.com>
--
-- Neovim 0.11 speaks the protocol sift speaks, so this is the whole
-- integration: no plugin, no dependency, and the findings, the quickfixes and
-- the formatting all come from `sift lsp'.
--
-- Put this in init.lua (or require it from there) and open a .scm, .el or
-- .lisp file.
--
--   gs   go to the definition of the ... no: see the keymaps below.

-- The command that starts the server.  Point it somewhere else if `sift' is
-- not on your PATH, e.g. { "/home/you/Hacking/sift/bin/sift", "lsp" }.
local sift = { "sift", "lsp" }

-- The dialects sift reads, by filetype and by extension: a buffer is started
-- once, at whichever of these it matches first.
local filetypes = { "scheme", "scheme.guile", "racket", "elisp", "emacs-lisp",
  "lisp" }
local extensions = { "scm", "ss", "sld", "sls", "rkt", "rktd", "rktl", "scrbl",
  "el", "lisp", "cl", "asd" }

local function matches(pattern)
  return vim.fn.glob(pattern, false, true)
end

-- The project root: the nearest directory holding a `sift.yaml', or a
-- repository, or the file's own directory.  The root is what makes one server
-- answer for a whole project -- the same configuration, the same rules -- and
-- a server per file would read the configuration once per file.
local function root_dir(bufnr)
  local name = vim.api.nvim_buf_get_name(bufnr)
  local start = vim.fs.dirname(name)
  local found = vim.fs.find({ "sift.yaml", "sift.yml", ".sift.yaml", ".git" },
    { upward = true, path = start })[1]
  return found and vim.fs.dirname(found) or start
end

local function on_attach(client, bufnr)
  local map = function(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = "sift: " .. desc })
  end
  -- Eglot's `M-x eglot-code-actions' has the same key in Emacs: the quickfixes
  -- are the ones `sift fix' applies.
  map("n", "<leader>ca", vim.lsp.buf.code_action, "code actions")
  map("n", "<leader>cf", function()
    vim.lsp.buf.format({ async = true })
  end, "format buffer")
  map("n", "K", vim.lsp.buf.hover, "what the rule wants")
  vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
end

-- Start the server for a buffer, once.  `vim.lsp.start' answers the existing
-- client when one already serves this root, which is what makes the call safe
-- to make from an autocmd.
local function start()
  local bufnr = vim.api.nvim_get_current_buf()
  vim.lsp.start({
    name = "sift",
    cmd = sift,
    root_dir = root_dir(bufnr),
    on_attach = on_attach,
  }, { bufnr = bufnr })
end

local group = vim.api.nvim_create_augroup("sift", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
  group = group,
  pattern = filetypes,
  callback = start,
})

vim.api.nvim_create_autocmd("BufReadPost", {
  group = group,
  callback = function(args)
    local extension = vim.fn.fnamemodify(args.file, ":e")
    if vim.tbl_contains(extensions, extension) then
      start()
    end
  end,
})

-- Formatting on save is a choice, and sift never writes a file it was not
-- asked to write: this asks it.
vim.api.nvim_create_autocmd("BufWritePre", {
  group = group,
  pattern = { "*.scm", "*.ss", "*.sld", "*.sls", "*.rkt", "*.rktd", "*.rktl",
    "*.scrbl", "*.el", "*.lisp", "*.cl", "*.asd" },
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})
