import { ThemeConfig } from 'antd'

import colors from '../colors.json'

export const theme: ThemeConfig = {
  components: {
    Layout: {
      headerBg: colors['secondary-1'],
      siderBg: '#fff',
      triggerBg: colors['primary-2'],
    },
    Menu: {
      itemActiveBg: colors['secondary-2'],
      itemColor: colors['link-1'],
      itemHoverBg: colors['secondary-2'],
      itemHoverColor: colors['link-1'],
      itemSelectedBg: colors['secondary-2'],
      itemSelectedColor: colors['primary-2'],
    },
    Input: {
      controlHeight: 38,
    },
    Button: {
      controlHeight: 38,
    },
    Typography: {
      colorLink: colors['link-1'],
    },
  },
}
