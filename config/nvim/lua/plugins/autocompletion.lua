return
{
	  -- lsp
	  {
		 "williamboman/mason.nvim",
		 lazy = false,
		 config = function()
		 require("mason").setup()
		 end,
	  },
	  {
		 "williamboman/mason-lspconfig.nvim",
		 lazy = false,
		 opts = {
		 auto_install = true,
		 },
	  },
	  {
		 "neovim/nvim-lspconfig",
		 lazy = false,
		 config = function()
		 local capabilities = require('cmp_nvim_lsp').default_capabilities()

		 local lspconfig = require("lspconfig")
		 lspconfig.tailwindcss.setup({
			capabilities = capabilities
		 })
		 lspconfig.jsonls.setup({
			capabilities = capabilities
		 })
		 lspconfig.bashls.setup({
			capabilities = capabilities
		 })
		 lspconfig.yamlls.setup({
			capabilities = capabilities
		 })
		 lspconfig.solargraph.setup({
			capabilities = capabilities
		 })
		 lspconfig.html.setup({
			capabilities = capabilities
		 })
		 lspconfig.lua_ls.setup({
			capabilities = capabilities
		 })

		 vim.keymap.set("n", "K", vim.lsp.buf.hover, {})
		 vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, {})
		 vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, {})
		 vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, {})
		 vim.keymap.set("n", "<leader>rn", vim.lsp.buf.code_action, {})
		 vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, {})
		 end,
	  },
	  -- completions
	  {
		 "L3MON4D3/LuaSnip",
		 dependencies = {
			"saadparwaiz1/cmp_luasnip",
			"rafamadriz/friendly-snippets",
		 },
	  },
	  {
		 "hrsh7th/nvim-cmp",
		 dependencies = {
			'neovim/nvim-lspconfig',
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-buffer',
			'hrsh7th/cmp-path',
			'hrsh7th/cmp-cmdline',
			'onsails/lspkind-nvim',
		 },
		 config = function()
			local cmp = require("cmp")
			require("luasnip.loaders.from_vscode").lazy_load()

			cmp.setup({
			   snippet = {
				  expand = function(args)
					 require("luasnip").lsp_expand(args.body)
				  end,
			   },
			   window = {
				  completion = cmp.config.window.bordered(),
				  documentation = cmp.config.window.bordered(),
			   },
			   mapping = cmp.mapping.preset.insert({
				  ["<C-b>"] = cmp.mapping.scroll_docs(-4),
				  ["<C-f>"] = cmp.mapping.scroll_docs(4),
				  ["<C-Space>"] = cmp.mapping.complete(),
				  ["<C-e>"] = cmp.mapping.abort(),
				  ["<CR>"] = cmp.mapping.confirm({ select = true }),
			   }),
			   sources = cmp.config.sources({
				  { name = "buffer" },
				  { name = "nvim_lsp" },
				  { name = "path" },
				  { name = "luasnip" }, -- For luasnip users.
			   }),
			   formatting = {
				  format = function(entry, vim_item)
					 -- Optional: Customize icons if you like
					 vim_item.kind = require('lspkind').presets.default[vim_item.kind]
					 vim_item.menu = ({
						nvim_lsp = '[LSP]',
						luasnip = '[Snippet]',
						path = '[Path]',
						buffer = '[Buffer]',
					 })[entry.source.name]
					 return vim_item
				  end,
			   },
			})
		 end,
	  }
}
