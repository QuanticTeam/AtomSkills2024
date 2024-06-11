import { UserOutlined } from '@ant-design/icons'
import { faker } from '@faker-js/faker'
import { Avatar, Badge, Button, Dropdown, Input, Layout } from 'antd'
import { useContext } from 'react'
import { Link } from 'react-router-dom'
import { AuthContext } from '~/shared/auth'
import { ROUTE_PATH_INDEX } from '~/shared/routng/constants'
import { colors } from '~/shared/styles'
import { Logo } from '~/shared/ui'

export function HeaderAuthorized() {
  const { logout } = useContext(AuthContext)

  return (
    <Layout.Header className="sticky top-0 p-0 h-20">
      <div className="flex items-center h-full border-b-4 border-primary-2">
        <div className="w-10 ml-5">
          <Link to={ROUTE_PATH_INDEX}>
            <Logo />
          </Link>
        </div>

        <div className="flex h-full grow justify-between items-center px-10">
          <Input.Search
            className="w-96"
            placeholder={faker.lorem.sentence(5)}
          />

          <Badge
            color={colors['primary-2']}
            count={12}
            offset={[-10, 0]}
            size="small"
          >
            <Dropdown
              menu={{
                items: [
                  {
                    label: (
                      <Button
                        type="link"
                        onClick={logout}
                      >
                        Выйти
                      </Button>
                    ),
                    key: '0',
                  },
                ],
              }}
              trigger={['click']}
            >
              <Avatar
                className="cursor-pointer"
                icon={<UserOutlined />}
                size={48}
              />
            </Dropdown>
          </Badge>
        </div>
      </div>
    </Layout.Header>
  )
}
