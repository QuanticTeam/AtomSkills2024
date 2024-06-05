import { Layout, Menu, MenuProps, SiderProps, Typography } from 'antd'

export interface SidebarProps {
  collapsed: boolean
  menuItems: MenuProps['items']
  onCollapse: SiderProps['onCollapse']
  selectedMenuItems: MenuProps['selectedKeys']
}

export function Sidebar({ collapsed, menuItems, onCollapse, selectedMenuItems }: SidebarProps) {
  return (
    <Layout.Sider
      className="!sticky w-24 border-r border-primary-2"
      collapsed={collapsed}
      collapsible
      onCollapse={onCollapse}
      trigger={
        <Typography.Text className="font-black text-lg text-secondary-2">
          {collapsed ? '>' : '<'}
        </Typography.Text>
      }
    >
      <Menu
        selectedKeys={selectedMenuItems}
        className="text-lg"
        mode="inline"
        items={menuItems}
      />
    </Layout.Sider>
  )
}
