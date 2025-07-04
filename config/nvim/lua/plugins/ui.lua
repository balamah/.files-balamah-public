local function readConfig(file_path)
    local config = {}
    for line in io.lines(file_path) do
        local key, value = line:match("^(%S+)%s*=%s*(%S+)$")
        if key and value then
            config[key] = value
        end
    end
    return config
end

local function getColorscheme(colorscheme)
   local cases = {
	  ["onedark-gray"] = "onedark",
	  ["onedark-cyan"] = "onedark"
   }

   return cases[colorscheme] or colorscheme
end

local settings = readConfig(os.getenv("HOME") .. "/.files-balamah/settings.conf")

return
{
	  -- mode line
	  {
		 'nvim-lualine/lualine.nvim',
		 dependencies = { 'nvim-tree/nvim-web-devicons' },
		 config = function ()
			-- if settings["enablePywal"] == 'yes' then
			   -- lualineTheme = 'pywal'
			-- else
			   -- lualineTheme = 'onedark'
			-- end
			lualineTheme = getColorscheme(settings["colorscheme"])

			require('lualine').setup {
			   options = {
				  icons_enabled = true,
				  theme = lualineTheme,
				  section_separators = { left = '', right = '' },
				  component_separators = { left = '', right = '' },
				  disabled_filetypes = {
					 statusline = {},
					 winbar = {},
				  },
				  ignore_focus = {},
				  always_divide_middle = true,
				  globalstatus = false,
				  refresh = {
					 statusline = 1000,
					 tabline = 1000,
					 winbar = 1000,
				  }
			   },
			   sections = {
				  lualine_a = {'mode'},
				  lualine_b = {'buffers', 'branch', 'diff', 'diagnostics'},
				  lualine_c = {'filename'},
				  lualine_x = {'encoding', 'fileformat', 'filetype'},
				  lualine_y = {'progress'},
				  lualine_z = {'location'}
			   },
			   inactive_sections = {
				  lualine_a = {},
				  lualine_b = {},
				  lualine_c = {'filename'},
				  lualine_x = {'location'},
				  lualine_y = {},
				  lualine_z = {}
			   },
			   tabline = {},
			   winbar = {},
			   inactive_winbar = {},
			   extensions = {}
			   }
		 end
	  },
	  -- colorscheme
	  {
		 'uZer/pywal16.nvim',
		 config = function ()
			if settings["colorscheme"] == "pywal" then
			   local pywal16 = require('pywal16')
			   pywal16.setup()
			end
		 end
	  },
	  {
		 'navarasu/onedark.nvim',
		 config = function ()
			if settings["colorscheme"] == "onedark-gray" or
			   settings["colorscheme"] == "onedark-cyan" then
			   require('onedark').setup {
				  style = 'dark'
			   }
			   require('onedark').load()
			end
		 end
	  },
	  {
		 "ellisonleao/gruvbox.nvim",
		 config = function ()
			if settings["colorscheme"] == "gruvbox" then
			   require("gruvbox").setup({
					 terminal_colors = true, -- add neovim terminal colors
					 undercurl = true,
					 underline = true,
					 bold = true,
					 italic = {
						strings = false,
						emphasis = false,
						comments = false,
						operators = false,
						folds = false,
					 },
					 strikethrough = true,
					 invert_selection = false,
					 invert_signs = false,
					 invert_tabline = false,
					 invert_intend_guides = false,
					 inverse = true, -- invert background for search, diffs, statuslines and errors
					 contrast = "", -- can be "hard", "soft" or empty string
					 palette_overrides = {},
					 overrides = {},
					 dim_inactive = false,
					 transparent_mode = false,
				  })
			   require('gruvbox').load()
			end
		 end
	  },
	  {
		  "Mofiqul/dracula.nvim",
		  config = function()
			  if settings["colorscheme"] == "dracula" then
				  require('dracula').load()
			  end
		  end
	  },
	  {
		 'Everblush/nvim', name = 'everblush',
		 config = function ()
			if settings["colorscheme"] == "everblush" then
			   require('everblush').setup({
				  -- Default options
				  override = {},
				  transparent_background = false,
				  nvim_tree = {
					 contrast = false,
				  },
			   })
			end
		 end
	  },
	  {
		 'shaunsingh/nord.nvim', name = 'nord',
		 config = function ()
			if settings["colorscheme"] == "nord" then
			   vim.cmd[[colorscheme nord]]
			end
		 end
	  },
	  -- dashboard
	  {
		 'nvimdev/dashboard-nvim',
		 event = 'VimEnter',
		 config = function()
			require('dashboard').setup {
			   theme = 'hyper',
			   config = {
				  weekHeader = {
					 enable = true,
				  },
				  header = {
					 [[ ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗ ]],
					 [[ ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║ ]],
					 [[ ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║ ]],
					 [[ ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║ ]],
					 [[ ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║ ]],
					 [[ ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝ ]],
				  },
				  shortcut = {
					 {
						desc = '󰚰 Update',
						group = '@property',
						action = 'Lazy update',
						key = 'u'
					 },
					 {
						desc = ' Sync',
                        group = '@property',
                        action = 'Lazy sync',
                        key = 's'
					 },
					 {
						desc = ' Clean',
                        group = '@property',
                        action = 'Lazy clean',
                        key = 'x'
					 },
					 {
						icon = ' ',
						icon_hl = '@variable',
						desc = 'Files',
						group = 'Label',
						action = 'Telescope find_files',
						key = 'f',
					 },
				  },
			   },
			}
		 end,
		 dependencies = { {'nvim-tree/nvim-web-devicons'}}
	  },
	  -- which key.
	  -- The plugin shows which keys to press
	  {
		 "folke/which-key.nvim",
		 event = "VeryLazy",
		 opts = {
			-- your configuration comes here
			-- or leave it empty to use the default settings
			-- refer to the configuration section below
		 },
		 keys = {
			{
			   "<leader>?",
			   function()
			   require("which-key").show({ global = false })
			   end,
			   desc = "Buffer Local Keymaps (which-key)",
			},
		 },
	  }
}
