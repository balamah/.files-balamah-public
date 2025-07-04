return
{
	  -- telescope
	  -- A fuzzy finder like councel in emacs but for neovim
	  {
		 "nvim-telescope/telescope-ui-select.nvim",
	  },
	  {
		 'nvim-telescope/telescope.nvim', tag = '0.1.8',
		 dependencies = { 'nvim-lua/plenary.nvim' },
		 config = function ()
			require("telescope").setup({
			   pickers = {
				  find_files = {
					 hidden = true,
					 theme = "ivy"
				  },
			   }
			})
			local telescope = require('telescope.builtin')
			vim.keymap.set(
			   'n', '<leader>b', telescope.buffers, { desc = 'Telescope buffers' }
			)
			vim.keymap.set(
			   'n',
			   '<leader>of',
			   telescope.find_files,
			   { desc = 'Telescope buffers' }
			)
			vim.keymap.set(
			   'n',
			   '<leader>osf',
			   telescope.live_grep,
			   { desc = 'Telescope buffers' }
			)
		 end
	  },
	  -- autopairs
	  {
		 'windwp/nvim-autopairs',
		 event = "InsertEnter",
		 config = true
	  },
	  -- comfortable switch
	  {
		  'christoomey/vim-tmux-navigator'
	  },
	  -- comment lines fast
	  {
		 'tpope/vim-commentary'
	  },
	  -- show css colors
	  {
		 'ap/vim-css-color'
	  },
	  -- org-mode.
	  -- Most of my README's are written in org-mode,
	  -- neovim doesn't have highlighting for org-mode by default
	  {
		 'nvim-orgmode/orgmode',
		 event = 'VeryLazy',
		 ft = { 'org' },
		 config = function()
			-- Setup orgmode
			require('orgmode').setup({
			   org_agenda_files = '~/orgfiles/**/*',
			   org_default_notes_file = '~/orgfiles/refile.org',
			})

			-- NOTE: If you are using nvim-treesitter with ~ensure_installed = "all"~ option
			-- add ~org~ to ignore_install
			-- require('nvim-treesitter.configs').setup({
			--   ensure_installed = 'all',
			--   ignore_install = { 'org' },
			-- })
		 end,
	  }
}
