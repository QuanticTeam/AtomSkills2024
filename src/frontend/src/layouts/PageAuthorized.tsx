import { AlignLeftOutlined, TableOutlined } from '@ant-design/icons'
import { Layout, MenuProps, Typography } from 'antd'
import { ReactNode, useEffect, useState } from 'react'

import { useLocation, useNavigate } from 'react-router-dom'
import { GuardAuthorized } from '~/shared/auth'
import { Breadcrumbs, BreadcrumbsProps, Footer, HeaderAuthorized, Sidebar } from '~/widgets'

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

interface PageAuthorizedProps {
  actions?: ReactNode
  breadcrumbs: BreadcrumbsProps['breadcrumbs']
  children: ReactNode
  title: ReactNode
}

export function PageAuthorized({ actions, children, title, breadcrumbs }: PageAuthorizedProps) {
  const [sidebarCollapsed, setSidebarCollapsed] = useState(true)
  const navigate = useNavigate()
  const location = useLocation()

  const [selectedSidebarMenuItems, setSelectedSidebarMenuItems] = useState<string[]>([])

  useEffect(() => {
    if (location.pathname.startsWith('/something')) setSelectedSidebarMenuItems(['/something'])
    if (location.pathname.startsWith('/lorem')) setSelectedSidebarMenuItems(['/lorem'])
  }, [location.pathname])

  const sidebarMenuItems: MenuItem[] = [
    getItem(
      <Typography.Link onClick={() => navigate('/something')}>Something</Typography.Link>,
      '/something',
      <TableOutlined className="!text-lg" />,
    ),
    getItem(
      <Typography.Link onClick={() => navigate('/lorem')}>Lorem</Typography.Link>,
      '/lorem',
      <AlignLeftOutlined className="!text-lg" />,
    ),
  ]

  return (
    <GuardAuthorized>
      <Layout className="h-screen">
        <HeaderAuthorized />

        <Layout>
          <Sidebar
            collapsed={sidebarCollapsed}
            menuItems={sidebarMenuItems}
            onCollapse={() => setSidebarCollapsed(!sidebarCollapsed)}
            selectedMenuItems={selectedSidebarMenuItems}
          />

          <Layout>
            <Layout.Content className="bg-gray-100">
              <div className="flex flex-col h-full">
                <Breadcrumbs breadcrumbs={breadcrumbs} />

                <div className="overflow-auto scroll-smooth grow mx-6">
                  <div className="mr-2 p-8 min-h-full bg-white rounded-md border border-slate-200">
                    <div className="flex items-center justify-between mb-10">
                      <div className="grow">
                        {typeof title === 'string' ? (
                          <Typography.Title className="!mb-0">{title}</Typography.Title>
                        ) : (
                          title
                        )}
                      </div>
                      <div className="flex items-center">{actions}</div>
                    </div>
                    {children}
                  </div>
                </div>
              </div>
            </Layout.Content>

            <Layout.Footer className="py-3.5">
              <Footer />
            </Layout.Footer>
          </Layout>
        </Layout>
      </Layout>
    </GuardAuthorized>
  )
}
