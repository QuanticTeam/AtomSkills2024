import {
  DesktopOutlined,
  FileOutlined,
  PieChartOutlined,
  TeamOutlined,
  UserOutlined,
} from '@ant-design/icons'
import { BreadcrumbProps, MenuProps, Layout, Typography, Menu, Breadcrumb } from 'antd'
import { ReactNode, useState } from 'react'
import { Logo } from '../common/Logo'

interface PageTemplageProps {
  breadcrumbs?: BreadcrumbProps['items']
  content: ReactNode
  footer?: ReactNode
  headerLeft?: ReactNode
  headerRight?: ReactNode
  title: ReactNode
}

type MenuItem = Required<MenuProps>['items'][number]

function getItem(
  label: React.ReactNode,
  key: React.Key,
  icon?: React.ReactNode,
  children?: MenuItem[],
): MenuItem {
  return {
    key,
    icon,
    children,
    label,
  } as MenuItem
}

const items: MenuItem[] = [
  getItem('Option 1', '1', <PieChartOutlined className="!text-lg" />),
  getItem('Option 2', '2', <DesktopOutlined className="!text-lg" />),
  getItem('User', 'sub1', <UserOutlined className="!text-lg" />, [
    getItem('Tom', '3'),
    getItem('Bill', '4'),
    getItem('Alex', '5'),
  ]),
  getItem('Team', 'sub2', <TeamOutlined className="!text-lg" />, [
    getItem('Team 1', '6'),
    getItem('Team 2', '8'),
  ]),
  getItem('Files', '9', <FileOutlined className="!text-lg" />),
]

export function PageTemplate({
  breadcrumbs,
  content,
  headerLeft,
  headerRight,
  title,
}: PageTemplageProps) {
  const [siderCollapsed, setSiderCollapsed] = useState(true)

  return (
    <Layout className="h-screen">
      <Layout.Header className="sticky top-0 p-0 border-b-4 border-salmon h-20">
        <div className="flex items-center h-full">
          <div className="w-10 ml-5">
            <Logo />
          </div>
          <div className="flex h-full grow justify-between items-center px-10">
            {headerLeft}
            {headerRight}
          </div>
        </div>
      </Layout.Header>

      <Layout>
        <Layout.Sider
          className="text-white !sticky w-24 border-r border-salmon"
          collapsed={siderCollapsed}
          collapsible
          onCollapse={() => setSiderCollapsed(!siderCollapsed)}
          trigger={
            <Typography.Text className="font-black text-lg text-peach">
              {siderCollapsed ? '>' : '<'}
            </Typography.Text>
          }
        >
          <Menu
            className="text-lg"
            defaultSelectedKeys={['1']}
            mode="inline"
            items={items}
          />
        </Layout.Sider>

        <Layout>
          <Layout.Content className="bg-gray-100">
            <div className="flex flex-col h-full">
              <Breadcrumb
                items={breadcrumbs}
                className="p-6"
              />

              <div className="overflow-auto scroll-smooth grow mx-6">
                <div className="mr-2 p-8 min-h-full bg-white rounded-md border border-slate-200">
                  <Typography.Title>{title}</Typography.Title>
                  <Typography.Text>{content}</Typography.Text>
                </div>
              </div>
            </div>
          </Layout.Content>

          <Layout.Footer className="py-4">
            <div className="flex items-center justify-center">
              <Typography.Text
                className="text-slate-500"
                code
              >
                Made with ❤️ by{' '}
                <a
                  className="!text-blue-400 !font-bold"
                  href="https://github.com/QuanticTeam/AtomSkills2024"
                >
                  QuanticTeam
                </a>
              </Typography.Text>
            </div>
          </Layout.Footer>
        </Layout>
      </Layout>
    </Layout>
  )
}
