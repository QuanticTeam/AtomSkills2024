import { ThemeConfig } from 'antd'

import colors from '../colors.json'

export const theme: ThemeConfig = {
  components: {
    Layout: {
      headerBg: '#6E4F4D',
      siderBg: '#fff',
      triggerBg: colors.salmon,
    },
    Menu: {
      itemColor: '#6E4F4D',
      itemActiveBg: '#FAF2DC',
      itemSelectedBg: '#FAF2DC',
      itemSelectedColor: '#ee968e',
    },
  },
}
